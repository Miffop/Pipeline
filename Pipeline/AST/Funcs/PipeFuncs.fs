namespace Pipeline.AST.Funcs

open Pipeline.AST

type PipeFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|PipeFuncCurried(arg)
and PipeFuncCurried(a:PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            f.Eval(a)
        |_->failwith "ожидалась функция"

type BindFunc(m:PMonad) = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|BindFuncCurried(m,arg)
and BindFuncCurried(m:PMonad,a:PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            m.Bind(a,f)
        |_->failwith "ожидалась функция"

type ThenFunc(m:PMonad) = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|ThenFuncCurried(m,arg)
and ThenFuncCurried(m:PMonad,a:PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        m.Then(a,arg)