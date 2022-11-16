namespace Pipeline.AST.Expressions

open Pipeline.AST

type MarkerDefExpression(name:string,length:int,s:StringImage option) = 
    inherit IExpression(s)
    override this.Eval(c) = 
        if length>0 then
            c.Def name this.Location (Func<|MarkerFunc(name,length))
        else
            c.Def name this.Location (Data<|Marker(name,[]))
        Func c.Monad.Return