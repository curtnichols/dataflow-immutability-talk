// This is example code for my talk "Tackling Mutability with Data Flow Grahs."
// Please note that it is based on an early spike rather than production code, so
// there may be irregularities.

open System
open System.Threading
open System.Threading.Tasks
open System.Threading.Tasks.Dataflow

// Utilties

let identity = fun x -> x

let log = // Don't print partial messages
    let guard = obj()
    fun s -> lock guard (fun () -> printfn "%s" s)

// These aren't very useful types; imagine there's a lot of data associated with them.
type NetworkData = { n: int }
type MutableFrame = { n: int }
type Frame = { n: int }

open GraphBinding

// ----------------------------------------------------------------------------
// Application-specific constructors for graph nodes (makeXXX).

let makeBuffer rhNode = (bindBuffer ()) rhNode

let makeDataAssembler rhNode =
    let pretendTransform state (data: NetworkData) =
        state := !state + 1
        { MutableFrame.n = !state }
    (bindTransformWithState (fun () -> ref 0) pretendTransform) rhNode

let makeImageProcessor rhNode =
    (bindTransform (fun (mf: MutableFrame) -> { Frame.n = mf.n })) rhNode

let makeModeler rhNode =
    (bindAction (fun f -> log (sprintf "Modeling frame %d" f.n))) rhNode

let makeSecretSauce rhNode =
    (bindAction (fun _ -> () (* it's secret *))) rhNode

let makeRecorder rhNode =
    (bindAction (fun f -> log (sprintf "Recording frame %d" f.n))) rhNode

// ----------------------------------------------------------------------------
// The graph is constructed from right to left, from leaf to root as
// ISourceBlock.LinkTo() requires the right-hand node to exist.
//
// Data flows through the graph from left to right:
//
//
// (network) --> [buffer] --> [data-assembler] --+
//                                               |
//          +------------------------------------+
//          V
//   [image-processor] -->  [tee] --> [modeler] ---> ·
//                            |
//                            +-----> [recorder] --> ·

let constructGraph = makeBuffer
                        << makeDataAssembler
                        << makeImageProcessor
                        << makeSecretSauce
                        << makeTee [ makeModeler << makeSecretSauce << terminate
                                     makeRecorder << terminate]

let quitWaitClean (root: Scaffold<_>) =
    let completeGraph (root: IDataflowBlock) = root.Complete ()
    let waitForGraphCompletion leafNodes = Task.WaitAll (leafNodes |> List.toArray)
    let cleanUpGraph (disposables: IDisposable list) =
        for d in disposables do
            if d <> null then d.Dispose()

    let target, leaves, disposables = root
    completeGraph target
    waitForGraphCompletion leaves
    cleanUpGraph disposables

// ----------------------------------------------------------------------------

[<EntryPoint>]
let main args =
    let graph = constructGraph ()
    let graphRoot, _, _ = graph

    seq { 1 .. 10 }
        |> Seq.iter (fun _ ->
                        graphRoot.Post ({NetworkData.n = 0}) |> ignore
                        Thread.Sleep (1000 / 30)) // ms
    quitWaitClean graph
    log "Done."

    0
