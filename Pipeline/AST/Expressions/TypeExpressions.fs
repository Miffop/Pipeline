namespace Pipeline.AST.Expressions

open Pipeline.AST

type MarkerDefExpression(name:string,length:int,s:StringImage option) = 
    inherit IExpression(s)
    override this.Eval(c) = 
        if length>0 then
            c.Def name this.Location (Func<|MarkerFunc(name,length))
        else
            c.Def name this.Location (Data<|Marker(name,[]))
        Func<|Identity()

type MonadDefExpression(name:string,def:IExpression,s:StringImage option) = 
    inherit IExpression(s)
    override this.Eval(c) =
        let mc = PContext(Some c,c.Monad)
        def.Eval(mc)|>ignore
        let bind = 
            match mc.Defenitions.["связка"].Head |> fst with
            |Func(f)->f
            |_->raise<|System.NotImplementedException()
        let ret = 
            match mc.Defenitions.["упаковщик"].Head |> fst with
            |Func(f)->f
            |_->raise<|System.NotImplementedException()
        let mon = CustomMonad(ret,bind)
        c.Def(name)(this.Location)(Data mon)
        Func<|Identity()
type MonadUseExpression(name:string,exp:IExpression,str:StringImage option) =
    inherit IExpression(str)
    override this.Eval(c) = 
        let monad = 
            match c.Find(name)(this.Location)with
            |Data(:?PMonad as m)->m
            |_->failwith "ожидалась монада"
        exp.Eval(PContext(Some c,monad))
