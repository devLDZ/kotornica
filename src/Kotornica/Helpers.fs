[<AutoOpen>]
module Kotornica.Helpers

open System

let notSupported message = raise (new NotSupportedException(message))

let notSupportedf fmt = Printf.ksprintf notSupported fmt 

let inline uint32ToByteArray value = [| 
    byte((value >>> 24) &&& 255u);
    byte((value >>> 16) &&& 255u);
    byte((value >>> 8) &&& 255u);
    byte(value &&& 255u) |]

type System.IO.BinaryReader with
    member this.ReadUInt32BigEndian() = 
        this.ReadBytes(4) |> Array.fold (fun state bt -> (state <<< 8) ||| uint32 bt) 0u

module Stream =
    open System.IO

    let writeByteArray array (stream:Stream) =
        stream.Write(array, 0, array.Length)
        stream

    let writeByte byte (stream:Stream) =
        stream.WriteByte byte
        stream

    let flush (stream:Stream) =
        stream.Flush()
