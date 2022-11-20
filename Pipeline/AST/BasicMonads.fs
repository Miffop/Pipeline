namespace Pipeline.AST


type IdentityMonad() = 
    inherit PMonad()
    let ret = Identity()
    override this.Return = ret
    override this.Bind(a,f) = f.Eval(a)

type CustomMonad(ret:PFunc,bind:PFunc) = 
    inherit PMonad()
    override this.Return = ret;
    override this.Bind(a,f) = 
        match bind.Eval(a) with
        |Func(bindA)->bindA.Eval(Func f)
        |_->failwithf "ожадалась карированная функция"