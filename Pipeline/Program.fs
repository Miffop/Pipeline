
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers

    let keyWords = ["define";"as";"of";"to";"for";"with";"do";"yield"]
    let tokenParser =         
        TokenParser([
            WordParser(keyWords)
            ArithmeticParser()
            IntParser()
            FloatParser()
            StringParser()
            CommentParser()
            WhitespaceParser()
        ])

open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions


[<EntryPoint>]
let main argv =

    
    let main1:Pipeline.AST.IExpression = 
        PipeExpression(true,
            DefExpression("x",LiteralExpression(20)),
            ApplyExpression(
                ApplyExpression(
                    LiteralExpression(ForLoopFunc(true)),
                    LiteralExpression([1..3])
                ),
                FuncExpression("i",
                    ApplyExpression(
                        ApplyExpression(
                            LiteralExpression(MulFunc()),
                            DefValueExpression("x")
                        ),
                        DefValueExpression("i")
                    )
                )
            )
        )
    let main2:Pipeline.AST.IExpression = 
        PipeExpression(true,
            DefExpression("Curry",FuncExpression("a",
                PipeExpression(true,
                    DefExpression("res",
                        ApplyExpression(LiteralExpression(SumFunc()),DefValueExpression("a"))
                    ),
                    DefValueExpression("res")
                )
            )),
            ApplyExpression(
                ApplyExpression(
                    DefValueExpression("Curry"),
                    LiteralExpression(2)
                ),
                LiteralExpression(3)
            )
        )
    main1.Eval(PContex())
    |> printfn "%A"
    
    (*
    let path = "./../../../Test.txt"
    let code = System.IO.File.ReadAllText(path)
    let tokens = tokens.tokenParser.Parse(code)
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    *)
    System.Console.ReadKey() |> ignore
    0