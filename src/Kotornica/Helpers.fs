[<AutoOpen>]
module Kotornica.Helpers

open System

let notSupported message = raise (new NotSupportedException(message))

let notSupportedf fmt = Printf.ksprintf notSupported fmt 

let toUint32 (byteArray : byte[]) =
    // TODO LO: make this more generic
    byteArray |> Array.fold (fun state bt -> (state >>> sizeof<byte>) ||| uint32 bt) 0u