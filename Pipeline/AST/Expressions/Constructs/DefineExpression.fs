namespace Pipeline.AST.Expressions.Constructs

open Pipeline.AST

type DefineExpression(defname:string,def:IExpression)= 
    interface IExpression with
        member this.Eval (c,o) = 
            c.Add defname <| def.Eval(c,null) 
            Data(o)