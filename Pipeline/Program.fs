
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers

    let keyWords = ["define";"as";"of";"to";"for";"with";"do";"yield";"if";"then";"else";"lazy"]
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
module simplifications = 
    open Pipeline.Parser.Simplifier
    open Pipeline.Parser.Simplifier.Simplifications
    let simplifier = 
        Simplifier([
            IfSimplification()
        ])

module expressions = 
    open Pipeline.Parser.Expressions
    open Pipeline.Parser.Expressions.ExpressionParsers
    open Pipeline.Parser.Expressions.OperationParsers

    let expParser = 
        ExpressionParser(
            [
                DefValueParser()
                LiteralParser()
                BraceBreakParser()
                LazyParser()
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
    
    (*for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    *)
    let tokens = simplifications.simplifier.Simplify(tokens,0,tokens.Length)
    printfn "simplified:"
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset

    let code = expressions.expParser.ParseCodeBlock(tokens,0)

    let c = PContext()
    c.Def("true")(Data true)
    c.Def("false")(Data false)
    c.Def("if")(Func<|Pipeline.AST.Funcs.CondFunc())
    c.Def("print")(Func<|Pipeline.AST.Funcs.PrintFunc())
    c.Def("eval")(Func<|Pipeline.AST.Funcs.EvalLazyFunc())
        
    code.Eval(c)
    |> printfn "%A"
    

    System.Console.ReadKey() |> ignore
    0