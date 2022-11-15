
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers

    let keyWords = ["пусть";"равно";"от";"до";"для";"с";"к";"на";"как";"в";"выполнить";"вернуть";"если";"то";"иначе";"ленивое"]
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
    let preparator = 
        Simplifier([
            MathsVariation()
            ConvertionVariation()
        ])
    let simplifier = 
        Simplifier([
            IfSimplification()
            DefineSimplification()
            LambdaSimplification()
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
                DefAndFuncParser()
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
    let code = System.IO.File.ReadAllText(path)+" "
    let tokens = tokens.tokenParser.Parse(code)
    
    (*for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    *)
    let tokens = simplifications.preparator.Simplify(tokens,0,tokens.Length)
    let tokens = simplifications.simplifier.Simplify(tokens,0,tokens.Length)
    printfn "simplified:"
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset

    let code = expressions.expParser.ParseCodeBlock(tokens,0)

    let c = PContext()
    c.Def("правда")(-1)(Data true)
    c.Def("ложь")(-1)(Data false)
    c.Def("тождество")(-1)(Func <|Identity())
    c.Merge <| PipelineReflectionImporter.ImportAsm(System.Reflection.Assembly.GetExecutingAssembly())
        
    let result = code.Eval(c)
    match result with
    |Func(f) -> 
        printf "\n\nпрограмма завершилась вернув функцию: %O\n" f
    |Data(d) ->
        printf "\n\nпрограмма завершилась вернув: %O\n" d
    

    System.Console.ReadKey() |> ignore
    0