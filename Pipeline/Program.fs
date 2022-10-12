[<EntryPoint>]
let main argv =


    let main:Pipeline.AST.IExpression = Pipeline.AST.CodeBlock([
        Pipeline.AST.Expressions.Constructs.DefineExpression("x",Pipeline.AST.Expressions.Inline.LiteralExpression(3))
        Pipeline.AST.Expressions.Inline.ValueExpression("x")
        Pipeline.AST.Expressions.Commands.PrintExpression()
    ])

    main.Eval(new Pipeline.AST.PContex(),null)
    |> printfn "%A"

    System.Console.ReadKey() |> ignore
    0