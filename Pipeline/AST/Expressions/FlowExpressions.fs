namespace Pipeline.AST.Expressions

open Pipeline.AST

//Appliction
type ApplyExpression(FuncExp:IExpression,argExp:IExpression,strImage:StringImage option) = 
    inherit IExpression(strImage)
    override this.Eval(c) =
        let func = FuncExp.Eval(c)
        match func,argExp.Eval(c) with
        |Func(F),arg->
            F.Eval(arg)
        |x,arg->
            raise <| NotAFunctionException(x,arg)

type PipeExpression(sudoPipe:bool,argExp:IExpression,FuncExp:IExpression,strImage:StringImage option) = 
    inherit IExpression(strImage)
    override this.Eval(c) = 
        let arg = argExp.Eval(c)
        match FuncExp.Eval(c),arg with
        |Func(aId),Func(bId) when aId = c.Monad.Return && bId = c.Monad.Return->aId.Eval(Func bId)
        |notFunc,Func(id) when sudoPipe && id=c.Monad.Return->
            notFunc
        |Func(F),arg->
             c.Monad.Bind(arg,F)
        |notFunc,arg->
            raise <| NotAFunctionException(notFunc,arg)

//DataOrFunc
type LiteralExpression(lit:PFunOrData,strImage:StringImage option) = 
    inherit IExpression(strImage)
    new(f:PFunc,strImage:StringImage option) = LiteralExpression(Func f,strImage)
    new(d:PData,strImage:StringImage option) = LiteralExpression(Data d,strImage)
    override this.Eval(c) = 
        lit
type ContextIsolationExpression(exp:IExpression) =
    inherit IExpression(None)
    override this.Eval(c) = 
        exp.Eval(PContext(Some c,c.Monad))
    override this.ToString() = exp.ToString()
    
type FuncExpression(x:string,exp:IExpression,strImage:StringImage option) = 
    inherit IExpression(strImage)
    override this.Eval(c) = 
        Func(ExpressionFunc(x,c,this.Location,exp))

//Defs
type DefExpression(defname:string,valExp:IExpression,strImage:StringImage option) = 
    inherit IExpression(strImage)
    override this.Eval(c) =
        let loc = this.Location
        let f =
            {
            new PFunc() with
            member this.Eval(x) = 
                c.Def defname loc x
                c.Monad.Return.Eval(Func<|Identity())
            }
        let fdef = 
            {
            new PFunc() with
            member this.Eval(x) = 
                c.Monad.Bind((valExp.Eval c),f) |> ignore
                c.Monad.Return.Eval(x)
            member this.Equals(o) = 
                match o with
                |x when (x = c.Monad.Return)->true
                |_->false
            }
            
        (Func fdef)

type DefValueExpression(defname:string,strImage:StringImage option) = 
    inherit  IExpression(strImage)
    override this.Eval(c) = 
        c.Find defname this.Location

//Lazy
type LazyExpression(exp:IExpression,strImage:StringImage option) = 
    inherit IExpression(strImage)
    override this.Eval(c) = 
        Data<|PLazy(exp,c)