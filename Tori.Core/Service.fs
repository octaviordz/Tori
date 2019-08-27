namespace Tori.Core

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Threading
open System.Diagnostics
open Comm

/// Client message id
type MessageId = uint16

type ServiceEvent = MessageSent of MessageId

type AwaitingResponse = MessageId

type internal State =
    { clients : TcpClient list
      receivedCount : int64
      sentCount : int64
      tagSequence : int
      await : Set<AwaitingResponse> }
    static member Default =
        { clients = []
          receivedCount = 0L
          sentCount = 0L
          tagSequence = 0
          await = Set.empty }

type internal LinkEvent =
    | ClientAdded of TcpClient list
    | ClientRemoved of TcpClient
    | LineReceived of Line list * TcpClient
    | LineSent of Line list * TcpClient list

type internal LinkCmd = SendLine of Line * TcpClient list

type internal NoteListAction =
    | ReceiveLine of Line list
    | SendNote of string

type internal LinkClientListAction =
    | Add of TcpClient list
    | Remove of TcpClient

type Command = SendNote of string

type internal Event =
    | ClientUpdated of LinkClientListAction
    | LineSent of Line list
    | LineRecieved of Line list
    | NoteSent of MessageId

type internal System() =
    static member Apply(state : State, event : Event) : State =
        match event with
        | ClientUpdated cmd ->
            match cmd with
            | Add clients ->
                let newState = { state with clients = clients @ state.clients }
                newState
            | Remove client ->
                let rec removeClient c lst =
                    match lst with
                    | h :: t when c = h -> t
                    | h :: t -> h :: removeClient c t
                    | _ -> []

                let newTcpClientList = state.clients |> removeClient client
                let newState = { state with clients = newTcpClientList }
                newState
        | LineRecieved lines ->
            let newState =
                { state with receivedCount = state.receivedCount + 1L }
            newState
        | LineSent lines ->
            let a = lines |> List.map (fun line -> line.tag)

            let set =
                a
                |> Set.ofList
                |> Set.union state.await

            let newState =
                { state with sentCount =
                                 state.sentCount + (List.length lines |> int64)
                             await = set }

            newState
        | NoteSent messageId ->
            let newState = { state with await = Set.add messageId state.await }
            newState

// foldEvents (apply, original_state, events[]) -> new_state
[<AutoOpen>]
module private Internal =
    let logTag = "server"

//Port
type internal ILink =
    //abstract member Property : int with get, set
    abstract LocalPort : int
    abstract SentCount : int64
    abstract Events : IObservable<LinkEvent>
    abstract Execute : LinkCmd -> unit
    abstract Start : unit -> unit
    abstract Stop : unit -> unit

// Adapter
type internal TcpLink() =
    // Initialize a server socket on the next available port.
    let tcpListener = TcpListener(IPAddress.Loopback, 0)
    //do tcpListener.Server.NoDelay <- true
    let acceptConnectionsCts = new CancellationTokenSource()
    let handleClientCts = new CancellationTokenSource()
    let startSignal = new ManualResetEventSlim(false)
    let events = new Event<LinkEvent>()
    let messagesMax = 10000
    let mutable lineSentEventPending = 0
    let mutable receivedIdx : int = 0
    let mutable received : Line [] = Array.zeroCreate messagesMax
    let mutable receivedCount = 0L
    let mutable sentCount = 0L
    let out = System.Collections.Generic.Dictionary<TcpClient, Line list>()

    let triggerEvent (event : LinkEvent) =
        try
            events.Trigger(event)
        with exn ->
            Log.Error(logTag, exn, "Error on trigger for {0}", MessageSent)

    let communicate (client : TcpClient) =
        async {
            let logTag =
                String.Format("{0} th-{1}", logTag, Thread.CurrentThread.Name)
            let inbuffer : byte [] = Array.zeroCreate 256
            let outbuffer : byte [] = Array.zeroCreate 256
            let mutable inIdx = 0

            let inputfun (stream : NetworkStream) : Async<Line> =
                async {
                    while inIdx < 4 do
                        let! i = stream.AsyncRead(inbuffer, inIdx, 4 - inIdx)
                        inIdx <- i + inIdx
                    Debug.Assert((inIdx = 4))
                    //for _idx in 0 .. idx - 1 do
                    //    sb.AppendFormat("{0:x2} ", buffer.[_idx]) |> ignore
                    //Log.Trace (sb.ToString())
                    let size = BitConverter.ToInt32(inbuffer, 0)
                    while inIdx < size do
                        let! i = stream.AsyncRead(inbuffer, inIdx, size - inIdx)
                        inIdx <- i + inIdx
                    Debug.Assert((inIdx = size))
                    //for _idx in 4 .. idx - 1 do
                    //    sb.AppendFormat("{0:x2} ", buffer.[_idx]) |> ignore
                    //Log.Trace (sb.ToString())
                    let line = Comm.deserialize inbuffer size
                    //let (kind, tag, note), nsize = Comm.deserialize inbuffer size
                    Interlocked.Increment(&receivedCount) |> ignore
                    inIdx <- 0
                    Log.Trace(logTag, "Received", (Line.Stringify line))
                    return line
                }

            let outputfun (stream : NetworkStream) (msgs : KindTagNote list) : Async<unit> =
                async {
                    for msg in msgs do
                        let line = Comm.serialize msg outbuffer
                        Log.Trace
                            (logTag, LogData(Line.Stringify line), "Sending")
                        do! stream.AsyncWrite(outbuffer, 0, line.size / 1<b8>)
                        Interlocked.Increment(&sentCount) |> ignore
                        //LogLine.builder
                        //    .Tag(logTag)
                        //    .Message("Sent")
                        //    .Data((Line.Stringify line))
                        //    .Build()
                        //|> Log.Trace
                        Log.Trace(logTag, LogData(Line.Stringify line), "Sent")
                }

            let rec ioLoop (stream : NetworkStream) =
                async {
                    let logTag = String.Format("{0} io-loop", logTag)
                    let! line = inputfun stream
                    //Store message in received
                    let mutable flag = true
                    while flag do
                        let currentIdx = receivedIdx
                        let mutable nextIdx = currentIdx + 1
                        if nextIdx >= messagesMax then nextIdx <- 1
                        if Interlocked.CompareExchange
                               (&receivedIdx, nextIdx, currentIdx) = currentIdx then
                            received.[nextIdx - 1] <- line
                            flag <- false
                    triggerEvent (LinkEvent.LineReceived([ line ], client))
                    let outlist =
                        match out.TryGetValue(client) with
                        | false, _ -> []
                        | true, l -> l
                    match outlist with
                    | [] -> ignore()
                    | lines ->
                        let kindTagNoteList =
                            lines
                            |> List.map
                                   (fun line -> (line.kind, line.tag, line.note))
                        try
                            Interlocked.Increment(&lineSentEventPending)
                            |> ignore
                            do! outputfun stream kindTagNoteList
                            out.[client] <- []
                            triggerEvent (LinkEvent.LineSent(lines, [ client ]))
                        finally
                            Interlocked.Decrement(&lineSentEventPending)
                            |> ignore
                    return! ioLoop stream
                }

            use stream = client.GetStream()
            do! ioLoop stream
            stream.Close()
        }

    let handleClient (client : TcpClient) =
        async {
            let ipEp = client.Client.RemoteEndPoint :?> IPEndPoint
            if String.IsNullOrEmpty Thread.CurrentThread.Name then
                Thread.CurrentThread.Name <- ipEp.Port.ToString()
            let logTag =
                String.Format
                    ("{0} {1} handleClient", logTag, Thread.CurrentThread.Name)
            try
                try
                    do! communicate client
                    Log.Trace(logTag, "After communicate")
                finally
                    Log.Trace(logTag, "Finally communicate")
                    client.Close()
            with
            | :? SocketException as exn ->
                Log.Trace(logTag, exn, "SocketException")
            | :? IOException as exn -> Log.Trace(logTag, exn, "IOException")
            | :? AggregateException as exn ->
                exn.Flatten().InnerExceptions
                |> Seq.iter
                       (fun exn ->
                       match exn with
                       | :? IOException as exn ->
                           Log.Trace
                               (logTag, exn, "AggregateException.IOException")
                       | _ ->
                           Log.Trace
                               (logTag, exn, "AggregateException unknown error"))
        }

    let acceptConnections =
        async {
            // Enter the listening loop.
            let mutable flag = true
            while flag do
                //Log.Trace(logTag, "Waiting for a client...")
                startSignal.Set() |> ignore
                let asyncTryAccept =
                    async {
                        try
                            let! c = Async.FromBeginEnd
                                         (tcpListener.BeginAcceptTcpClient,
                                          tcpListener.EndAcceptTcpClient)
                            return Choice1Of3 c
                        with
                        | :? SocketException as exn when (exn.SocketErrorCode = SocketError.Interrupted) ->
                            Log.Trace(logTag, exn, "Cancellation requested")
                            return Choice2Of3(exn :> Exception)
                        | :? SocketException as exn ->
                            Log.Error(logTag, exn, "Unexpected")
                            return Choice3Of3(exn :> Exception)
                        | :? ObjectDisposedException as exn when (exn.ObjectName = "System.Net.Sockets.Socket") ->
                            Log.Trace(logTag, exn, "Cancellation requested")
                            return Choice2Of3(exn :> Exception)
                        | exn ->
                            Log.Error(logTag, exn, "Unknown error")
                            return Choice3Of3 exn
                    }
                match! asyncTryAccept with
                | Choice3Of3 _ -> flag <- false
                | Choice2Of3 _ -> flag <- false
                | Choice1Of3 client ->
                    //Log.Trace(logTag, "Incoming client.")
                    let clients =
                        seq {
                            yield client
                            while tcpListener.Pending() do
                                yield tcpListener.AcceptTcpClient()
                        }
                        |> List.ofSeq
                    clients
                    |> Seq.map (fun client ->
                           let ipEp =
                               client.Client.RemoteEndPoint :?> IPEndPoint
                           Log.Trace
                               (logTag, "Client connected. Remote port {0}",
                                ipEp.Port)
                           handleClient client)
                    |> Async.Parallel
                    |> Async.Ignore
                    |> fun handleClientsComputation ->
                        (Async.Start
                             (handleClientsComputation, handleClientCts.Token))
                    triggerEvent (LinkEvent.ClientAdded clients)
        }

    member __.Events() = events.Publish :> IObservable<LinkEvent>

    member __.Execute(cmd : LinkCmd) =
        match cmd with
        | SendLine(line, clients) ->
            for client in clients do
                match out.TryGetValue(client) with
                | false, _ -> out.[client] <- [ line ]
                | true, list -> out.[client] <- line :: list

    member __.LocalTcpPort =
        let ipEp = tcpListener.LocalEndpoint :?> IPEndPoint
        ipEp.Port

    interface ILink with
        member self.LocalPort = self.LocalTcpPort
        member __.SentCount = sentCount
        member self.Events = self.Events()
        member self.Execute cmd = self.Execute(cmd)

        member self.Start() =
            tcpListener.Start()
            Log.Trace(logTag, "Server local tcp {0}", self.LocalTcpPort)
            Async.Start(acceptConnections, acceptConnectionsCts.Token)
            startSignal.Wait() |> ignore
            startSignal.Reset() |> ignore

        member __.Stop() =
            let logTag =
                String.Format("{0} {1}", logTag, Thread.CurrentThread.Name)
            Log.Info(logTag, "Stopping.")
            Log.Trace(logTag, "Stopping accept-connections.")
            acceptConnectionsCts.Cancel()
            tcpListener.Stop()
            while lineSentEventPending > 0 do
                Thread.CurrentThread.Join(100) |> ignore
            Log.Trace(logTag, "Stopping handle-client.")
            handleClientCts.Cancel()
            Log.Trace(logTag, "Stopped.")

[<AllowNullLiteral>]
type Server() =
    let lockObj = Object()
    let mutable timeline : State list = []
    let mutable serverName = String.Empty
    let linkControl : ILink = TcpLink() :> ILink
    let events : Event<ServiceEvent> = new Event<ServiceEvent>()

    let triggerEvent (event : ServiceEvent) =
        try
            events.Trigger(event)
        with exn -> Log.Error(logTag, exn, "Error on trigger for {0}", event)

    let linkControlSubscriber =
        linkControl.Events.Subscribe(fun event ->
            //Log.Trace(logTag, ":timeline {0} (unsafe-zone)", (List.length timeline))
            lock lockObj (fun __ ->
                //Log.Trace(logTag, ":timeline {0}", (List.length timeline))
                let templine = timeline

                let state =
                    match templine with
                    | h :: _ -> h
                    | [] -> State.Default

                let new' =
                    match event with
                    | ClientAdded clients ->
                        // try to accept/add clients on success create an event
                        let event = ClientUpdated(Add clients)
                        Some(System.Apply(state, event))
                    | ClientRemoved _ ->
                        Log.Info(logTag, "Client removed")
                        None
                    | LineReceived(lines, client) ->
                        // try to process on success create an system event
                        let await = templine.Head.await

                        let checkAwait kind tag =
                            match kind with
                            | LineKind.Rwrite ->
                                if await |> Set.contains tag then
                                    triggerEvent (MessageSent tag)
                            | _ -> ()

                        let processLine line =
                            let kindValue = (int) line.kind
                            let nsize = line.noteSize / 1<b8>

                            let rkind, rnote =
                                match line.kind with
                                | LineKind.Twrite ->
                                    LineKind.Rwrite, nsize.ToString()
                                | _ -> enum (kindValue + 1), line.note

                            let respLine =
                                { Line.Default with kind = rkind
                                                    tag = line.tag
                                                    note = rnote }

                            linkControl.Execute(SendLine(respLine, [ client ]))
                            checkAwait line.kind line.tag

                        lines |> List.iter processLine
                        let event = LineRecieved lines
                        Some(System.Apply(state, event))
                    | LinkEvent.LineSent(lines, client) ->
                        let event = Event.LineSent lines
                        Some(System.Apply(state, event))

                match new' with
                | None -> ()
                | Some newstate ->
                    //let sb = StringBuilder()
                    //Printf.bprintf sb "\n =============================="
                    //Printf.bprintf sb "\n Event %A" a
                    //Printf.bprintf sb "\n Init-state %A" state
                    //Printf.bprintf sb "\n State %A" newstate
                    //Printf.bprintf sb "\n =============================="
                    //Log.Trace(logTag, sb.ToString())
                    timeline <- newstate :: templine))

    member __.ServerName
        with get () = serverName
        and set (value) = serverName <- value

    member __.Events() = events.Publish :> IObservable<ServiceEvent>
    member __.ReceivedCount = timeline.Head.receivedCount
    member __.SentCount = timeline.Head.sentCount
    member __.LinkSentCount = linkControl.SentCount
    member __.LocalTcpPort = linkControl.LocalPort
    member __.Start() = linkControl.Start()

    member __.Stop() =
        linkControl.Stop()
        linkControlSubscriber.Dispose()

    member __.Execute(cmd : Command) =
        lock lockObj (fun __ ->
            let templine = timeline

            let state =
                match templine with
                | h :: _ -> h
                | [] -> State.Default
            match cmd with
            | SendNote note ->
                let mutable tagSequence = templine.Head.tagSequence
                let tag = (uint16 tagSequence)
                let clients = templine.Head.clients

                let line =
                    { Line.Default with kind = LineKind.Twrite
                                        tag = tag
                                        note = note }
                Interlocked.Increment(&tagSequence) |> ignore
                linkControl.Execute(SendLine(line, clients))
                let event = Event.NoteSent tag
                let templine = System.Apply(state, event) :: templine
                // timeline.append state
                timeline <- templine
                tag)

    member __.PrintState() =
        let templine = timeline
        printfn "=============================="
        match templine with
        | [] -> printfn "%A" []
        | x :: xs -> printfn "%A" x
        printfn "=============================="

    member self.BroadCast(message : string) : MessageId =
        // A message is sent until we have a confirmation that at least one client got it.
        let resp = self.Execute(SendNote message)
        resp
