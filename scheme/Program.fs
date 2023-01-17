open FsUnit
open NUnit.Framework
open Parser

[<Test>]
let ``test hello`` () = 1 + 1 |> should equal 2

[<EntryPoint>]
let main argv =
    let input = if argv.Length = 0 then "" else argv.[0]
    let result = readExpr input
    printfn "%s\n" result
    0