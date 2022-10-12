namespace Pipeline.AST.Expressions.Inline

open Pipeline.AST

type ValueExpression(defname:string) = 
    interface IExpression with
        member this.Eval c = 
            defname
            |> c.Find