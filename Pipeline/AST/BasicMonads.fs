namespace Pipeline.AST


type IdentityMonad() = 
    inherit PMonad()
    let ret = Identity()
    override this.Return = ret
    override this.Bind(a,f) = f.Eval(a)