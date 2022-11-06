
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers

    let keyWords = ["define";"as";"of";"to";"for";"with";"do";"yield";"if";"then";"else"]
    let tokenParser =         
        TokenParser([
            WordParser(keyWords)
            ArithmeticParser()
            BraceParser()
            IntParser()
            FloatParser()
            StringParser()
            CommentParser()
            WhitespaceParser()
        ])

module expressions = 
    open Pipeline.Parser.Expressions
    open Pipeline.Parser.Expressions.ExpressionParsers
    open Pipeline.Parser.Expressions.OperationParsers

    let expParser = 
        ExpressionParser(
            [
                LiteralParser()
                BraceBreakParser()
            ],
            [
                MathOperationParser()
                PipeOperationParser()
            ]
        )


open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions


[<EntryPoint>]
let main argv =
    
    
    let path = "./../../../Test.txt"
    let code = System.IO.File.ReadAllText(path)
    let tokens = tokens.tokenParser.Parse(code)
    
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    

    let code = expressions.expParser.ParseCodeBlock(tokens,0)

    code.Eval(PContext())
    |> printfn "%A"


    System.Console.ReadKey() |> ignore
    0