namespace Pipeline.AST.Expressions.Constructs

open Pipeline.AST

type DefineExpression(defname:string,def:IExpression)= 
    interface IExpression with
        member this.Eval (c,o) = 
            c.Def defname <| def.Eval(c,o) 
            Data(o)