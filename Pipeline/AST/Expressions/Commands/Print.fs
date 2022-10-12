namespace Pipeline.AST.Expressions.Commands

open Pipeline.AST

type PrintExpression() = 
    interface IExpression with
        member this.Eval (c,o) = 
            printfn "%O" o
            Data(null)