[<AutoOpen>]
module Logging

open System
open System.Text

type public LogLevel =
    | Trace = 0
    | Debug = 1
    | Info = 2
    | Warn = 3
    | Error = 4
    | Fatal = 5

[<Struct>]
type LogData(value : string) =
    member __.Value = value

type LogLine =
    | Message of string
    //| MessageRecord of {|msg:string; data:string|}
    | MessageFunc of Func<string>

//type LogLineBuilder () =
//    let logLine = LogLine()
//    member __.Tag(value:string)
//    member __.Message(value:string)
//    member __.Message(format:string, [<ParamArray>]arguments)
//    member __.Data(value:string)
//    member __.Build(value:string) =
module private Internal =
    let string (logLine : LogLine) =
        match logLine with
        | Message m -> m
        | MessageFunc func -> func.Invoke()

    let writeLine (tag : string, logLine : LogLine, arguments : obj array,
                   data : string, exn : exn) =
        let sb = StringBuilder(250)
        //"O" ISO8601 RFC3339
        //let inst = now.ToString("HH:mm:ss.ff")
        let messageTemplate = string (logLine)
        // #log/level #log/tag #log/message #log/data
        // :info [tag1 tag2] "message" {:k1 d1 :k2 d2} #{exn}
        match (isNull exn, arguments) with
        | false, [||] ->
            sb.AppendFormat("[{0}] \"", tag) |> ignore
            sb.Append(messageTemplate) |> ignore
            sb.AppendFormat("\" {0} #{{\"{1}\"}}", data, exn) |> ignore
            Console.Error.WriteLine(sb.ToString())
            Console.Error.Flush()
        | true, [||] ->
            sb.AppendFormat("[{0}] ", tag) |> ignore
            sb.Append(messageTemplate) |> ignore
            sb.AppendFormat(" {0}", data) |> ignore
            Console.WriteLine(sb.ToString())
        | false, arguments ->
            sb.AppendFormat("[{0}] \"", tag) |> ignore
            sb.AppendFormat(messageTemplate, arguments) |> ignore
            sb.AppendFormat("\" {0} #{{\"{1}\"}}", data, exn) |> ignore
            Console.Error.WriteLine(sb.ToString())
            Console.Error.Flush()
        | true, arguments ->
            sb.AppendFormat("[{0}] ", tag) |> ignore
            sb.AppendFormat(messageTemplate, arguments) |> ignore
            sb.AppendFormat(" {0}", data) |> ignore
            Console.WriteLine(sb.ToString())

    let log (logLevel : LogLevel) (tag : string) (logLine : LogLine)
        (arguments : obj array) (data : string) (exn : exn) =
        match logLevel with
        | LogLevel.Trace -> writeLine (tag, logLine, arguments, data, exn)
        | LogLevel.Debug -> writeLine (tag, logLine, arguments, data, exn)
        | LogLevel.Info -> writeLine (tag, logLine, arguments, data, exn)
        | LogLevel.Warn -> writeLine (tag, logLine, arguments, data, exn)
        | LogLevel.Error -> writeLine (tag, logLine, arguments, data, exn)
        | LogLevel.Fatal -> writeLine (tag, logLine, arguments, data, exn)
        | _ -> writeLine (tag, logLine, arguments, data, exn)

let mutable private logfun = Internal.log

let SetLogFun(logFun : LogLevel -> string -> LogLine -> obj array -> string -> exn -> unit) =
    logfun <- logFun
    ()

type LogFormat<'T>(logLevel : LogLevel, tag : string, exn : exn, logLine : LogLine, [<ParamArray>] arguments : obj array, data : string) =
    new(logLevel, tag : string, message : string, data : string) =
        LogFormat(logLevel, tag, null, (Message message), [||], data)
    new(logLevel, tag : string, logLine : LogLine, data : string) =
        LogFormat(logLevel, tag, null, logLine, [||], data)
    member __.LogLevel = logLevel
    member __.Tag = tag
    member __.Exception = exn
    member __.LogLine = logLine
    member __.Arguments = arguments
    member __.Data = data

type TraceLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Trace, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data : string) =
        TraceLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data : string) =
        TraceLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data : string) =
        TraceLine(tag, null, logLine, [||], data)

type DebugLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Debug, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data : string) =
        DebugLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data : string) =
        DebugLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data : string) =
        DebugLine(tag, null, logLine, [||], data)

type InfoLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Info, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data) =
        InfoLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data) =
        InfoLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data) =
        InfoLine(tag, null, logLine, [||], data)

type WarnLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Warn, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data : string) =
        WarnLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data : string) =
        WarnLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data : string) =
        WarnLine(tag, null, logLine, [||], data)

type ErrorLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Error, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data : string) =
        ErrorLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data : string) =
        ErrorLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data : string) =
        ErrorLine(tag, null, logLine, [||], data)

type FatalLine<'T>(tag : string, exn : exn, logLine : LogLine, arguments : obj array, data : string) =
    inherit LogFormat<'T>(LogLevel.Fatal, tag, exn, logLine, arguments, data)
    new(tag : string, exn : exn, message : string, data : string) =
        FatalLine(tag, exn, (Message message), [||], data)
    new(tag : string, message : string, data : string) =
        FatalLine(tag, null, (Message message), [||], data)
    new(tag : string, logLine : LogLine, data : string) =
        FatalLine(tag, null, logLine, [||], data)

let log (logFormat : LogFormat<'T>) : unit =
    logfun logFormat.LogLevel logFormat.Tag logFormat.LogLine
        logFormat.Arguments logFormat.Data logFormat.Exception

[<Sealed>]
type Log =
    static member Trace(tag : string, data : LogData, msg : string,
                        [<ParamArray>] arguments : obj array) =
        log (TraceLine(tag, null, (Message msg), arguments, data.Value))
    static member Trace(tag : string, msg : string,
                        [<ParamArray>] arguments : obj array) =
        log (TraceLine(tag, null, (Message msg), arguments, null))
    static member Trace(tag : string, msg : string, data : string) =
        log (TraceLine(tag, null, (Message msg), [||], data))
    static member Trace(tag : string, msg : string) =
        log (TraceLine(tag, msg, null))
    static member Trace(tag : string, exn : exn, msg : string,
                        [<ParamArray>] arguments : obj array) =
        log (TraceLine(tag, exn, (Message msg), arguments, null))
    static member Trace(tag : string, exn : exn, msg : string) =
        log (TraceLine(tag, exn, msg, null))
    static member Trace(tag : string, logLine : LogLine) =
        log (TraceLine(tag, logLine, null))
    static member Debug(tag : string, data : string, msg : string,
                        [<ParamArray>] arguments : obj array) =
        log (DebugLine(tag, null, (Message msg), arguments, data))
    static member Debug(tag : string, msg : string, data : string) =
        log (DebugLine(tag, null, (Message msg), [||], data))
    static member Debug(tag : string, msg : string) =
        log (DebugLine(tag, msg, null))
    static member Debug(tag : string, exn : exn, msg : string) =
        log (DebugLine(tag, exn, msg, null))
    static member Debug(tag : string, logLine : LogLine) =
        log (DebugLine(tag, logLine, null))
    static member Info(tag : string, data : LogData, msg : string,
                       [<ParamArray>] arguments : obj array) =
        log (InfoLine(tag, null, (Message msg), arguments, data.Value))
    static member Info(tag : string, msg : string) =
        log (InfoLine(tag, msg, null))
    static member Info(tag : string, exn : exn, msg : string) =
        log (InfoLine(tag, exn, msg, null))
    static member Info(tag : string, msg : string,
                       [<ParamArray>] arguments : obj array) =
        log (InfoLine(tag, null, (Message msg), arguments, null))
    static member Info(tag : string, logLine : LogLine) =
        log (InfoLine(tag, logLine, null))
    static member Warn(tag : string, exn : exn, msg : string,
                       [<ParamArray>] arguments : obj array) =
        log (WarnLine(tag, exn, (Message msg), arguments, null))
    static member Warn(tag : string, msg : string) =
        log (WarnLine(tag, msg, null))
    static member Warn(tag : string, exn : exn, msg : string) =
        log (WarnLine(tag, exn, msg, null))
    static member Warn(tag : string, logLine : LogLine) =
        log (WarnLine(tag, logLine, null))
    static member Error(tag : string, exn : exn, msg : string,
                        [<ParamArray>] arguments : obj array) =
        log (ErrorLine(tag, exn, (Message msg), arguments, null))
    static member Error(tag : string, msg : string) =
        log (ErrorLine(tag, msg, null))
    static member Error(tag : string, exn : exn, msg : string) =
        log (ErrorLine(tag, exn, msg, null))
    static member Error(tag : string, logLine : LogLine) =
        log (ErrorLine(tag, logLine, null))
    static member Fatal(tag : string, msg : string) =
        log (FatalLine(tag, msg, null))
    static member Fatal(tag : string, exn : exn, msg : string) =
        log (FatalLine(tag, exn, msg, null))
    static member Fatal(tag : string, logLine : LogLine) =
        log (FatalLine(tag, logLine, null))
