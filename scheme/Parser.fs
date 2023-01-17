module Parser

open FsUnit
open NUnit.Framework
open FParsec
open LispTypes

type LispState = unit
type Parser<'t> = Parser<'t, LispState>

let pSymbol: Parser<_> = anyOf "!#$%&|*+-/:<=>?@^_~"

let notQuoteChar = noneOf (Seq.toList "\"")
let unquotedString = manyChars notQuoteChar
let betweenQuotes = between (pstring "\"") (pstring "\"")

let parseString: Parser<LispVal> = betweenQuotes unquotedString |>> LispString

let parseAtom =
    pipe2 (letter <|> pSymbol) (manyChars (letter <|> digit <|> pSymbol)) (fun s rest ->
        let atom = sprintf "%c%s" s rest

        match atom with
        | "#t" -> LispBool true
        | "#f" -> LispBool false
        | _ -> LispAtom atom)

let parseNumber: Parser<_> = pint64 |>> LispNumber

let parseExpr, parseExprRef = createParserForwardedToRef ()

let parseList = sepBy parseExpr spaces1 |>> LispList

let parseDottedList =
    pipe2 (sepEndBy parseExpr spaces1) (pchar '.' >>. spaces >>. parseExpr) (fun head tail -> LispDottedList(head, tail))

let parseQuoted =
    pchar '\'' >>. parseExpr
    |>> (fun expr -> [ LispAtom "quote"; expr ] |> LispList)

parseExprRef
:= choice
    [ parseAtom
      parseString
      parseNumber
      parseQuoted
      (between (pchar '(') (pchar ')') (attempt parseList <|> parseDottedList)) ]


let readExpr input =
    match run (spaces >>. parseExpr) input with
    | Failure(_, err, _) -> sprintf "No match: %s" (err.ToString())
    | Success _ -> "Found value"

let checkResult v r =
    match r with
    | ParserResult.Success(e, _, _) -> e |> should equal v
    | _ -> Assert.Fail "parse failed"

let checkParseFailed r =
    match r with
    | ParserResult.Success(_, _, _) -> Assert.Fail("Expect parse fail")
    | _ -> ()

let p = readExpr "'(1 3 (\"this\" \"one\"))'"
printfn "%A" p

let p1 = readExpr "\"a string\""
printfn "%A" p1

let p2 = readExpr "(+ 2 2)"
printfn "%A" p2

[<Test>]
let ``parse atom rest`` () =
    run parseAtom "#t" |> checkResult (LispBool true)
    run parseAtom "test" |> checkResult (LispAtom "test")
    run parseAtom "5" |> checkParseFailed
