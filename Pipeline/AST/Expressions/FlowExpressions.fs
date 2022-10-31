namespace Pipeline.AST.Expressions

open Pipeline.AST

type ApplyExpression(FuncExp:IExpression,argExp:IExpression) = 
    inherit IExpression()
    override this.Eval(c) =
        let arg = argExp.Eval(c)
        match FuncExp.Eval(c) with
        |Func(F)->
            F.Eval(arg)
        |x->
            raise <| NotAFunctionException(x,arg)
type LiteralExpression(lit:PFunOrData) = 
    inherit IExpression()
    new(f) = LiteralExpression(Func f)
    new(d) = LiteralExpression(Data d)
    override this.Eval(c) = 
        lit
type ContextIsolationExpression(exp:IExpression) =
    inherit IExpression()
    override this.Eval(c) = 
        exp.Eval(PContex(Some c))

type FuncExpression(x:string,exp:IExpression) = 
    inherit IExpression()
    override this.Eval(c) = 
        Func(ExpressionFunc(x,c,exp))


type DefExpression(defname:string,valExp:IExpression) = 
    inherit IExpression()
    override this.Eval(c) =
        c.Def defname (valExp.Eval c)
        Func(Identity())
