namespace Pipeline.AST


type Identity() = 
    inherit PFunc()
    override this.Eval(a) = a
    override this.ToString() = "identity"

type ExpressionFunc(x:string,c:PContex,exp:IExpression) = 
    inherit PFunc()
    override this.Eval(arg) = 
        let c = PContex(Some c)
        c.Def x arg
        exp.Eval(c)
    override this.ToString() = 
        sprintf "fun of %s %s" (x) (exp.ToString())

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