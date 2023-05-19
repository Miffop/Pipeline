

module matcher = 
    open Pipeline.AST
    let reductions = 
        [
            Reductions.unlambda
            
            Reductions.Maths.arithmetic
            Reductions.Maths.comparison
            Reductions.Maths.logic
            Reductions.Maths.bitwise
            Reductions.Maths.errors
            
            Reductions.Data.pair
            Reductions.Data.list
            Reductions.Data.maybe
            Reductions.Data.IOMonad
            
            Reductions.combinators
            Reductions.errorHandling
        ]
        |>Match.choose

open Pipeline.AST
open Pipeline.Parser

[<EntryPoint>]
let main argv =
    
    

    //printfn "лексер..."
    let path = (*argv.[0];*)"./../../../zProg/test.txt"
    let code = System.IO.File.ReadAllText(path)+" \n"

    if code.Contains "\t" then 
        failwith "за табы бан"
    let chars = 
        code
        |>List.ofSeq
        |>Parser.run(Parser.terminated<|Indentation.text ' ' '\n')
        |>Option.map(fst)
        |>Option.defaultValue []
    let toks = 
        chars
        |>Parser.run(TokenParser.Tokenizer)
        |>Option.map(fst)
        |>Option.defaultValue []
    toks
    |>Seq.iter(printfn "%A")


    
    printfn "парсер..."
    let code = 
        toks
        |>Parser.run(ExpressionParser.Expression)
        |>Option.map(fst>>fst)
        |>Option.defaultValue(Error(null))
    
    
    printfn "%A" code
    
    printfn "выполняется..."
    
    
    let result =
        Execution.execution matcher.reductions code
    
    result
    |>Seq.iter(printfn "[%O]")
    
    match Seq.tryLast result with
    |Some(d) ->
        printf "\n\nпрограмма завершилась вернув %A\n" d
    |None ->
        printfn "ошибка"

    
    System.Console.ReadKey() |> ignore
    0