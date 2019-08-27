module ClientServerTests

open System
open System.Net.Sockets
open System.Diagnostics
open Tori.Core
open System.Threading
open Comm
open System.Text
open Xunit
open Xunit.Abstractions

let string (logLine : LogLine) =
    match logLine with
    | Message m -> m
    | MessageFunc func -> func.Invoke()

let writeLine (output : ITestOutputHelper)
    (tag : string, logLine : LogLine, arguments : obj array, data : string,
     exn : exn) =
    let sb = StringBuilder(250)
    let messageTemplate = string logLine
    match (isNull exn, arguments) with
    | false, [||] ->
        sb.AppendFormat("[{0}] \"", tag) |> ignore
        sb.Append(messageTemplate) |> ignore
        sb.AppendFormat("\" {0} #{{\"{1}\"}}", data, exn) |> ignore
        output.WriteLine(sb.ToString())
    | true, [||] ->
        sb.AppendFormat("[{0}] ", tag) |> ignore
        sb.Append(messageTemplate) |> ignore
        sb.AppendFormat(" {0}", data) |> ignore
        output.WriteLine(sb.ToString())
    | false, arguments ->
        sb.AppendFormat("[{0}] \"", tag) |> ignore
        sb.AppendFormat(messageTemplate, arguments) |> ignore
        sb.AppendFormat("\" {0} #{{\"{1}\"}}", data, exn) |> ignore
        output.WriteLine(sb.ToString())
    | true, arguments ->
        sb.AppendFormat("[{0}] ", tag) |> ignore
        sb.AppendFormat(messageTemplate, arguments) |> ignore
        sb.AppendFormat(" {0}", data) |> ignore
        output.WriteLine(sb.ToString())

let logFun (output : ITestOutputHelper) =
    let log (logLevel : LogLevel) (tag : string) (logLine : LogLine)
        (arguments : obj array) (data : string) (exn : exn) =
        match logLevel with
        | LogLevel.Trace ->
            writeLine output (tag, logLine, arguments, data, exn)
        | LogLevel.Debug ->
            writeLine output (tag, logLine, arguments, data, exn)
        | LogLevel.Info -> writeLine output (tag, logLine, arguments, data, exn)
        | LogLevel.Warn -> writeLine output (tag, logLine, arguments, data, exn)
        | LogLevel.Error ->
            writeLine output (tag, logLine, arguments, data, exn)
        | LogLevel.Fatal ->
            writeLine output (tag, logLine, arguments, data, exn)
        | _ -> writeLine output (tag, logLine, arguments, data, exn)
    log

let setOutputStream (output) =
    match output with
    | null -> ()
    | output -> Logging.SetLogFun(logFun output)

let setThreadName (name) =
    if String.IsNullOrEmpty Thread.CurrentThread.Name then
        Thread.CurrentThread.Name <- name

type ClientServer(output : ITestOutputHelper) =
    let output = output
    do setOutputStream output
    do setThreadName ("thread-test")
    let logTag = "client"

    [<Fact>]
    member __.``Single client one message``() =
        printfn "============================================================"
        printfn "Single client one message"
        printfn "============================================================"
        let server = Server()
        server.ServerName <- "Single client one message"
        server.Start()
        let bytes = Array.zeroCreate 256
        let clientId = "test-client"
        use client = new TcpClient()
        client.Connect("127.0.0.1", server.LocalTcpPort)
        use stream = client.GetStream()
        let tag = 1us
        let line = serialize (LineKind.Twrite, tag, clientId) bytes
        let size = line.size / 1<b8>
        stream.Write(bytes, 0, size)
        let sb = StringBuilder()
        // size[4] type[1] tag[2] nsize[2] note
        sb.AppendFormat
            ("Sent {{:size {0} :type {1} :tag {2} :note \"{3}\"}} {4}\t ",
             line.size, line.kind, tag, clientId, Environment.NewLine) |> ignore
        for idx in 0..size - 1 do
            sb.AppendFormat("{0:x2} ", bytes.[idx]) |> ignore
        Log.Info(logTag, sb.ToString())
        let i = stream.Read(bytes, 0, 4)
        Debug.Assert((i = 4))
        let size = BitConverter.ToInt32(bytes, 0)
        let i = stream.Read(bytes, 4, size)
        Debug.Assert((i + 4 = size))
        let line = Comm.deserialize bytes size
        let rnsize = Int32.Parse(line.note) * 1<b8>
        Log.Info
            (logTag, "Received {{:size {0} :type {1} :tag {2} :note \"{3}\"}}",
             size, line.kind, line.tag, line.note)
        Trace.Assert((rnsize = line.noteSize))
        stream.Flush()
        client.Close()
        server.Stop()
        Assert.Equal(1L, server.ReceivedCount)

    [<Fact>]
    member __.``Single client multiple messages``() =
        printfn "============================================================"
        printfn "Single client multiple message"
        printfn "============================================================"
        let server = Server()
        server.Start()
        let bytes = Array.zeroCreate 256
        let mcount = 10
        use client = new TcpClient()
        //client.NoDelay <- true
        client.Connect("127.0.0.1", server.LocalTcpPort)
        use stream = client.GetStream()
        let random = Random()
        for i in [ 1..mcount ] do
            let rand = random.Next()
            let tag = (uint16 i)
            let line = serialize (LineKind.Twrite, tag, rand.ToString()) bytes
            stream.Write(bytes, 0, line.size / 1<b8>)
            // size[4] type[1] tag[2] nsize[2] note
            Log.Info
                (logTag, "Sent {{:size {0} :type {1} :tag {2} :note \"{3}\"}}",
                 line.size, line.kind, line.tag, line.note)
            let i = stream.Read(bytes, 0, 4)
            Debug.Assert((i = 4))
            let size = BitConverter.ToInt32(bytes, 0)
            let i = stream.Read(bytes, 4, size)
            Debug.Assert((i + 4 = size))
            let respLine = Comm.deserialize bytes size
            Log.Info
                (logTag,
                 "Received {{:size {0} :type {1} :tag {2} :note \"{3}\"}}",
                 respLine.size, respLine.kind, respLine.tag, respLine.note)
            let respNoteSize = Int32.Parse(respLine.note) * 1<b8>
            Trace.Assert((respNoteSize = line.noteSize))
        Log.Info(logTag, "Closing connection.")
        stream.Flush()
        client.Close()
        Log.Info(logTag, "Stopping server.")
        server.Stop()
        Log.Info(logTag, "ReceivedCount {0}.", server.ReceivedCount)
        Log.Info(logTag, "SentCount {0}.", server.SentCount)
        Assert.Equal((int64 mcount), server.ReceivedCount)
        Assert.Equal((int64 mcount), server.SentCount)

    [<Fact>]
    member __.``Multiple clients single message per client``() =
        printfn "============================================================"
        printfn "Multiple clients single message per client"
        printfn "============================================================"
        let clientMax = 7
        let server = Server()
        server.Start()
        let client (clientIdx : int) =
            async {
                let bytes = Array.zeroCreate 256
                let tag = 1us
                let clientId = String.Format("{0}", clientIdx)
                let note = clientId
                let line = serialize (LineKind.Twrite, tag, note) bytes
                let logTag = String.Format("client {0}", logTag)
                Log.Info
                    (logTag,
                     "============================================================")
                Log.Info
                    (logTag, ":managedThreadId {0}",
                     Thread.CurrentThread.ManagedThreadId)
                use client = new TcpClient()
                //Log.Info(logTag, "Connecting...", clientId)
                client.Connect("127.0.0.1", server.LocalTcpPort)
                Log.Info(logTag, "Conected.")
                use stream = client.GetStream()
                do! stream.AsyncWrite(bytes, 0, line.size / 1<b8>)
                Log.Info
                    (logTag,
                     "Sent {{:size {0} :type {1} :tag {2} :note \"{3}\"}}",
                     line.size, line.kind, line.tag, line.note)
                let! i = stream.AsyncRead(bytes, 0, 4)
                Debug.Assert((i = 4))
                let size = BitConverter.ToInt32(bytes, 0)
                let! i = stream.AsyncRead(bytes, 4, size)
                Debug.Assert((i + 4 = size))
                let respLine = Comm.deserialize bytes size
                Log.Info
                    (logTag,
                     "Received {{:size {0} :type {1} :tag {2} :note \"{3}\"}}",
                     respLine.size, respLine.kind, respLine.tag, respLine.note)
                let rnsize = Int32.Parse(respLine.note) * 1<b8>
                Trace.Assert((rnsize = line.noteSize))
                Log.Info(logTag, "Closing connection.")
                stream.Flush()
                client.Close()
            }
        [ for i in 1..clientMax -> client i ]
        |> Async.Parallel
        |> Async.Ignore
        |> Async.RunSynchronously
        Log.Info(logTag, "Stopping server.")
        Log.Info(logTag, "LinkSentCount {0}.", server.LinkSentCount)
        server.Stop()
        Log.Info(logTag, "ReceivedCount {0}.", server.ReceivedCount)
        Log.Info(logTag, "SentCount {0}.", server.SentCount)
        Assert.Equal((int64 clientMax), server.ReceivedCount)
        Assert.Equal((int64 clientMax), server.SentCount)
