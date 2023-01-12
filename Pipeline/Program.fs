
module tokens = 
    open Pipeline.Parser.Tokens
    open Pipeline.Parser.Tokens.TokenParsers

    let keyWords = ["пусть";"от";"до";"для";"с";"к";"на";"как";"в";"выполнить";"вернуть";"если";"то";"иначе";"ленивое";"маркер";"монада";"применить"]
    let tokenParser =         
        TokenParser([
            WordParser(keyWords)
            QuoteWordParser()
            WildCardParser()
            ArithmeticParser()
            BraceParser()
            IntParser()
            FloatParser()
            StringParser()
            CommentParser()
            WhitespaceParser()
            ComparisionParser()
            LogicParser()
            PipeParser()
            ListParser()
        ])
module simplifications = 
    open Pipeline.Parser.Simplifier
    open Pipeline.Parser.Simplifier.Simplifications
    let preparator = 
        Simplifier([
            MathsVariation()
            ConvertionVariation()
            RangeVariation()
        ])
    let simplifier = 
        Simplifier([
            IfSimplification()
            ForSimplification()
            DefineSimplification()
            LambdaSimplification()
            MarkerSimplification()
            MonadSimplification()
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
                MarkerParser()
                MonadParser()
            ],
            [
                MathOperationParser()
                PipeOperationParser()
                ComparisonOperationParser()
                LogicOperationParser()
                ExtraOperationParser()
            ]
        )


open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions


[<EntryPoint>]
let main argv =
    
    
    printfn "лексер..."
    let path = "./../../../zProg/LogicsTest.txt"
    let code = System.IO.File.ReadAllText(path)+" \n "
    let tokens = tokens.tokenParser.Parse(code)
    
    (*for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    *)
    
    printfn "преобразователь 1..."
    let tokens = simplifications.preparator.Simplify(tokens,0,tokens.Length)
    
    printfn "преобразователь 2..."
    let tokens = simplifications.simplifier.Simplify(tokens,0,tokens.Length)
    (*printfn "simplified:"
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    *)
    
    
    printfn "парсер..."
    let code = expressions.expParser.ParseCodeBlock(tokens,0)

    let n10 = 
        [1..10]
        |>Seq.map(fun x->Data x)

    let c = PContext(IdentityMonad())
    c.Def("правда")(-1)(Data true)
    c.Def("ложь")(-1)(Data false)
    c.Def("тождество")(-1)(Func <|Identity())
    c.Merge <| PipelineReflectionImporter.ImportAsm(System.Reflection.Assembly.GetExecutingAssembly())
    
    printfn "выполняется..."
    let result = 
        match code.Eval(PContext(Some c,c.Monad)) with
        |Func(:?Pipeline.Extra.Types.IOType as io) ->
            io.Perform(Pipeline.Extra.Types.RealWorld()) |> snd,"[ВводВывод] и"
        |x->x,""
    
    
    match result with
    |Func(f),x -> 
        printf "\n\nпрограмма завершилась вернув %s функцию: %O\n" x f
    |Data(d),x ->
        printf "\n\nпрограмма завершилась вернув %s %O\n" x (Data d)
    

    System.Console.ReadKey() |> ignore
    0