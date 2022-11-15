namespace Pipeline.AST


type Identity() = 
    inherit PFunc()
    override this.Eval(a) = a
    override this.ToString() = "тождество"
type ExpressionFunc(x:string,c:PContext,loc:int,exp:IExpression) = 
    inherit PFunc()
    override this.Eval(arg) = 
        let c = PContext(Some c)
        c.Def x loc arg
        exp.Eval(c)
    override this.ToString() = 
        sprintf "от %s %s" (x) (exp.ToString())

[<AbstractClass>]
type SeparatorFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(d) -> this.EvalData d
        |Func(f) -> this.EvalFunc f
    abstract EvalData:d:PData->PFunOrData
    abstract EvalFunc:f:PFunc->PFunOrData
    default this.EvalData d = 
        raise<|System.NotImplementedException();
    default this.EvalFunc f = 
        raise<|System.NotImplementedException();