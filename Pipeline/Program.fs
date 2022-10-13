(*
open Pipeline.AST
open Pipeline.AST.Expressions.Inline
open Pipeline.AST.Expressions.Commands
open Pipeline.AST.Expressions.Constructs
*)
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers
    let tokenParser =         
        TokenParser([
            WordParser()
            ArithmeticParser()
            IntParser()
            FloatParser()
            StringParser()
            CommentParser()
            WhitespaceParser()
        ])


[<EntryPoint>]
let main argv =

    (*
    let main:Pipeline.AST.IExpression = Pipeline.AST.CodeBlock([
        DefineExpression("x",LiteralExpression("20"))
        ForExpression("i",None,Some<|RangeExpression(LiteralExpression(0),LiteralExpression(9),None),true,CodeBlock([
            SumExpression(IntExpression(ValueExpression("x")),ValueExpression("i"))
            PrintExpression()
        ]))
    ])
    main.Eval(new PContex(),null)
    |> printfn "%A"
    *)
    let path = "./../../../Test.txt"
    let code = System.IO.File.ReadAllText(path)
    let tokens = tokens.tokenParser.Parse(code)
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    
    System.Console.ReadKey() |> ignore
    0