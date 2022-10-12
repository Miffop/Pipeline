namespace Pipeline.AST.Expressions.Commands

open Pipeline.AST

type PrintExpression() = 
    interface IExpression with
        member this.Eval c = 
            printfn "%O" c.Object
            Data(null)