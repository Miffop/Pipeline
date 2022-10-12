namespace Pipeline.AST.Expressions.Inline

open Pipeline.AST

type ValueExpression(defname:string) = 
    interface IExpression with
        member this.Eval (c,o) = 
            defname
            |> c.Find