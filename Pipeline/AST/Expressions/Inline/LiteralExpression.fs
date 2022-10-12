namespace Pipeline.AST.Expressions.Inline

open Pipeline.AST

type LiteralExpression(o:obj) = 
    interface IExpression with
        member this.Eval (c,object) = 
            Data(o)