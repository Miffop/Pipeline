
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
                LiteralParser()
                BraceBreakParser()
                DefAndFuncParser()
                DefValueParser()
            ],
            [
                MathOperationParser()
                PipeOperationParser()
                ComparisonOperationParser()
                LogicOperationParser()
                ExtraOperationParser()
            ]
        )

module matcher = 
    open Pipeline.AST
    let reductions = 
        [
            Reductions.Maths.arithmetic
            Reductions.Maths.comparison
            Reductions.Maths.logic
            Reductions.Maths.bitwise
            Reductions.Maths.errors
            
            Reductions.Data.pair
            Reductions.Data.list
            Reductions.Data.maybe
            Reductions.Data.IOMonad
            
            Reductions.unlambda
            Reductions.combinators
            Reductions.errorHandling
        ]
        |>Match.choose

open Pipeline.AST


[<EntryPoint>]
let main argv =
    
    

    //printfn "лексер..."
    let path = (*argv.[0];*)"./../../../zProg/test.txt"
    let code = System.IO.File.ReadAllText(path)+" \n "
    let tokens = tokens.tokenParser.Parse(code)
    
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    
    
    //printfn "преобразователь 1..."
    let tokens = simplifications.preparator.Simplify(tokens,0,tokens.Length)
    
    //printfn "преобразователь 2..."
    let tokens = simplifications.simplifier.Simplify(tokens,0,tokens.Length)
    printfn "simplified:"
    for t in tokens do
        printfn "(%s;\t %s\tL %i\tM %i\tO %i)" t.Type t.Content t.Line t.Margin t.Offset
    
    
    
    //printfn "парсер..."
    let code = expressions.expParser.ParseCodeBlock(tokens,0)

    //c.Merge <| PipelineReflectionImporter.ImportAsm(System.Reflection.Assembly.GetExecutingAssembly())
    

   

    //printfn "выполняется..."
    let result =
        Execution.execution matcher.reductions code
        
    result
    |>Seq.iter(printfn "[%O]")
    
    
    match Seq.last result with
    |d ->
        printf "\n\nпрограмма завершилась вернув %A\n" d
    

    System.Console.ReadKey() |> ignore
    0