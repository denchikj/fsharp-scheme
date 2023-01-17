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
        runRepl()
    else
        argv |> List.ofArray |> runOne 
    0
