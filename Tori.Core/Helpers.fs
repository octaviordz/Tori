[<AutoOpen>]
module Helpers

open System

type Comm.Line with
    static member inline Stringify(line : Comm.Line) =
        sprintf "{:type %A :tag %i :note \"%s\"}" line.kind line.tag line.note
    static member inline Stringify(kind, tag, note) =
        sprintf "{:type %A :tag %i :note \"%s\"}" kind tag note

// https://github.com/tpetricek/TryJoinads/blob/master/src/FSharp.Joinads/Async.fs
// http://fssnip.net/8x
type Microsoft.FSharp.Control.Async with

    /// Starts the specified operation using a new CancellationToken and returns
    /// IDisposable object that cancels the computation. This method can be used
    /// when implementing the Subscribe method of IObservable interface.
    static member StartDisposable(op : Async<unit>) =
        let ct = new System.Threading.CancellationTokenSource()
        Async.Start(op, ct.Token)
        { new IDisposable with
              member x.Dispose() = ct.Cancel() }

    /// Creates an asynchronous workflow that non-deterministically returns the
    /// result of one of the two specified workflows (the one that completes
    /// first). This is similar to Task.WaitAny.
    static member Choose(a, b) : Async<'T> =
        Async.FromContinuations(fun (cont, econt, _ccont) ->
            // Results from the two
            let result1 = ref (Choice1Of3())
            let result2 = ref (Choice1Of3())
            let handled = ref false
            let lockObj = new obj()
            let synchronized f = lock lockObj f

            // Called when one of the workflows completes
            let complete() =
                let op =
                    synchronized (fun () ->
                        // If we already handled result (and called continuation)
                        // then ignore. Otherwise, if the computation succeeds, then
                        // run the continuation and mark state as handled.
                        // Only throw if both workflows failed.
                        match !handled, !result1, !result2 with
                        | true, _, _ -> ignore
                        | false, (Choice2Of3 value), _
                        | false, _, (Choice2Of3 value) ->
                            handled := true
                            (fun () -> cont value)
                        | false, Choice3Of3 e1, Choice3Of3 e2 ->
                            handled := true
                            (fun () ->
                            econt
                                (AggregateException
                                     ("Both clauses of a choice failed.",
                                      [| e1; e2 |])))
                        | false, Choice1Of3 _, Choice3Of3 _
                        | false, Choice3Of3 _, Choice1Of3 _
                        | false, Choice1Of3 _, Choice1Of3 _ -> ignore)
                op()

            // Run a workflow and write result (or exception to a ref cell
            let run resCell workflow =
                async {
                    try
                        let! res = workflow
                        synchronized (fun () -> resCell := Choice2Of3 res)
                    with e -> synchronized (fun () -> resCell := Choice3Of3 e)
                    complete()
                }

            // Start both work items in thread pool
            Async.Start(run result1 a)
            Async.Start(run result2 b))
