open FsUnit
open NUnit.Framework
open Parser
open Eval

[<Test>]
let ``test hello`` () = 1 + 1 |> should equal 2

[<EntryPoint>]
let main argv =
    let result =
        argv |> Array.tryHead |> Option.defaultValue "" |> readExpr |> Result.bind eval

    match result with
    | Result.Ok v -> printfn "evaluated: %s" (v.ToString())
    | Result.Error e -> printfn "error: %s" (e.ToString())

    0
