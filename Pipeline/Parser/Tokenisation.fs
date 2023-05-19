namespace Pipeline.Parser

[<RequireQualifiedAccess>]
type Token = 
    |Int of int
    |Float of float
    |String of string
    |Word of string
    |Op of string
    |BraceOpen
    |BraceClose

module TokenParser = 
    open Indentation.Ops
    open System


    let inline private sign p = 
        p <??> ((fun x-> -x) <^ Indentation.token '-')
    let intToken = 
        let uint = 
            (List.fold(sprintf"%s%c")"">>int)
            <^>Indentation.some(Indentation.satisfy Char.IsDigit)
        Token.Int<^>sign uint
    let floatToken = 
        let ufloat = 
            (fun x y->x@ y |> List.fold(sprintf"%s%c")"" |> float)
            <^>Indentation.some(Indentation.satisfy System.Char.IsDigit)
            <*>((fun x y->x::y)<^>Indentation.token '.'<*>Indentation.any(Indentation.satisfy Char.IsDigit))
        Token.Float<^>sign ufloat
    let stringToken = 
        let char =
            let slashes = 
                ['"','"';'\\','\\']
                |>Map.ofList
                |>Indentation.mapTokens
            [
                lazy(Indentation.token '\\'*>slashes)
                lazy(Indentation.satisfy((<>)'"'))
            ]
            |>Parser.choose
        (string>>Token.String)<^>Indentation.pack '"' char '"'
    let braceToken = 
        [
            lazy(Token.BraceOpen <^Indentation.token '(')
            lazy(Token.BraceClose<^Indentation.token ')')
        ]
        |>Parser.choose
    let stringPattern s = 
        (List.fold(sprintf"%s%c")"")<^>Indentation.pattern(List.ofSeq s)
    let OpToken = 
        [
            lazy(stringPattern "+")
            lazy(stringPattern "-")
            lazy(stringPattern "*")
            lazy(stringPattern "//")
            lazy(stringPattern "/")
            lazy(stringPattern "\\\\")
            lazy(stringPattern "\\")
            lazy(stringPattern "...")
            lazy(stringPattern "..")
            lazy(stringPattern ".")
            lazy(stringPattern ",,,")
            lazy(stringPattern ",,")
            lazy(stringPattern ",")
            lazy(stringPattern "!=")
            lazy(stringPattern "=")
            lazy(stringPattern ":")
            lazy(stringPattern ";")
        ]
        |>Parser.choose
        |>(<^>)Token.Op
    let wordOps = 
        [
            "или"
            "и"
            "болшр"
            "меншр"
            "болш"
            "менш"

            "если"
            "то"
            "иначе"

            "пусть"
            "рек"
        ]
    let wordToken =
        (List.fold(sprintf"%s%c"))<^>(string<^>Indentation.satisfy Char.IsLetter)<*>Indentation.any(Indentation.satisfy Char.IsLetterOrDigit)
        |>(<^>)(function x when List.contains x wordOps -> Token.Op x | x-> Token.Word x )
    let AllTokens = 
        [
            lazy(floatToken)
            lazy(intToken)
            lazy(stringToken)
            lazy(braceToken)
            lazy(OpToken)
            lazy(wordToken)
        ]
        |>Parser.choose
    open Parser.Ops
    
    let Tokenizer = 
        let Void = Indentation.any(Indentation.satisfy Char.IsWhiteSpace)
        Void*>Parser.any(AllTokens<*Void)
        