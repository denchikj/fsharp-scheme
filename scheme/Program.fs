open FsUnit
open NUnit.Framework
open Parser
open Eval
open Repl

[<Test>]
let ``test hello`` () = 1 + 1 |> should equal 2

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        runRepl ()
    else
        let result =
            argv |> Array.tryHead |> Option.defaultValue "" |> readExpr |> Result.bind eval

        match result with
        | Ok v -> printfn "%s" (v.ToString())
        | Error e -> printfn "error: %s" (e.ToString())

    0
