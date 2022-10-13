
open Pipeline.AST
open Pipeline.AST.Expressions.Inline
open Pipeline.AST.Expressions.Commands
open Pipeline.AST.Expressions.Constructs

[<EntryPoint>]
let main argv =

    let main:Pipeline.AST.IExpression = Pipeline.AST.CodeBlock([
        DefineExpression("x",LiteralExpression("20"))
        ForExpression("i",None,Some<|RangeExpression(LiteralExpression(0),LiteralExpression(9),None),true,CodeBlock([
            SumExpression(IntExpression(ValueExpression("x")),ValueExpression("i"))
            PrintExpression()
        ]))
    ])

    main.Eval(new PContex(),null)
    |> printfn "%A"


    System.Console.ReadKey() |> ignore
    0