module Kotornica.Mercurial

open System
open System.Diagnostics
open System.IO
open System.Text

type private Channel = 
    | Output
    | Error
    | Result
    | Debug
    | Input
    | Line

type private ServerResponse =
    | ServerResponse of Channel * string

type private Command =
    | Summary

type Capability =
    | GetEncoding
    | RunCommand
    override __.ToString() = 
        match __ with
        | GetEncoding -> "getencoding"
        | RunCommand -> "runcommand"
    static member TryParse = function
        | "getencoding" -> Some GetEncoding
        | "runcommand" -> Some RunCommand
        | _ -> None

type ServerInfo =
    {
        Capabilties: Capability array;
        Encoding: string;
        Pid: int;
    }

type ServerEnv = 
    { 
        HgPath: string; Encoding: Encoding; 
    } 
    static member Default = { HgPath = "hg"; Encoding = Encoding.UTF8 }

type private ServerContext = 
    {
        Input: Stream;
        Output: BinaryReader;
        Encoding: Encoding;
    }

let private startHg env repositoryPath =
    let psi = new ProcessStartInfo(
                FileName = env.HgPath,
                CreateNoWindow = true,
                UseShellExecute = false,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                RedirectStandardOutput = true)

    psi.EnvironmentVariables.Add("HGENCODING", env.Encoding.WebName)
    psi.Arguments <-sprintf @"serve --cmdserver pipe --cwd %s --repository %s" repositoryPath repositoryPath
    Process.Start(psi)

let private consume (reader:BinaryReader) =
    let channel = 
        match reader.ReadChar() with
        | 'o' -> Output
        | 'e' -> Error
        | 'r' -> Result
        | 'd' -> Debug
        | 'i' -> Input
        | 'l' -> Line
        | other -> notSupported "The channel %c is not supported" other
    
    let dataLength = reader.ReadUInt32BigEndian()
    if dataLength > uint32(Int32.MaxValue) then notSupported "server response is too long"
    let stringData = new String(reader.ReadChars(int dataLength))
    ServerResponse (channel, stringData)

let private summary server =
    let summaryBytes = server.Encoding.GetBytes "summary"
    server.Input 
        |> Stream.writeByteArray (server.Encoding.GetBytes "runcommand\n")
        |> Stream.writeByteArray (uint32 summaryBytes.Length |> uint32ToByteArray)
        |> Stream.writeByteArray summaryBytes
        |> Stream.flush

    let rec consumeUntil () = 
        match consume server.Output with
        | ServerResponse(Output, data) -> 
            printfn "\t%A" data
            consumeUntil()
        | ServerResponse(Result, data) -> printfn "EOF: %A" data
        | other -> failwithf "Unexpected output: %A" other

    consumeUntil()

let private init server =
    let getHeaderAndValue (str:string) = 
        match str.Split([| ':' |], 2) with
        | [| header; value |] -> (header, value.Trim())
        | _ -> failwithf "Malformed line %s" str

    let updateInfo (info:ServerInfo) = function
        | ("capabilities", capabilities:string) -> { info with Capabilties = capabilities.Split(' ') |> Array.choose Capability.TryParse }
        | ("encoding", encoding) -> { info with Encoding = encoding }
        | ("pid", pid) -> { info with Pid = Int32.Parse(pid) }
        | _ -> info

    let (ServerResponse(channel, data)) = consume server.Output
    if channel <> Output then failwith "Encountered an unexpected channel"

    data.Split('\n') 
        |> Array.map getHeaderAndValue 
        |> Array.fold updateInfo { Encoding = null; Pid = -1; Capabilties = [||] }

type HgServer(env, repositoryPath) =
    let hgProc = startHg env repositoryPath
    let io = { 
        Input = hgProc.StandardInput.BaseStream; 
        Output = new BinaryReader(hgProc.StandardOutput.BaseStream, env.Encoding);
        Encoding = env.Encoding;
    }
    let info = init io

    let mutable disposed = false

    let dispose() = 
        if not disposed then
            io.Input.Dispose()
            io.Output.Dispose()
            hgProc.Dispose()
            disposed <- true

    new(repositoryPath) = new HgServer(ServerEnv.Default, repositoryPath)

    member __.Info = info

    member __.Summary() = summary io

    interface IDisposable with
        member __.Dispose() = dispose()