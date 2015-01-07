open System
open System.IO
open System.Diagnostics
open System.Threading.Tasks

open Kotornica.Mercurial
[<EntryPoint>]
let main argv = 
    use srv = new HgServer(@"H:\Development\Projects\Kotornica\test_repo")
    printfn "INFO: %A" srv.Info

    srv.Summary()

    Console.ReadLine() |> ignore
    0 // return an integer exit code
