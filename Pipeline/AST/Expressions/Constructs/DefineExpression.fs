namespace Pipeline.AST.Expressions.Constructs

open Pipeline.AST

type DefineExpression(defname:string,def:IExpression)= 
    interface IExpression with
        member this.Eval c = 
            c.Add defname (def.Eval c)
            Data(c.Object)