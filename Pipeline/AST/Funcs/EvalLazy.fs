namespace Pipeline.AST.Funcs

open Pipeline.AST

type EvalLazyFunc() = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        | Data(:?PLazy as l) ->l.Value
        |_->raise<|WrongTypeException("eval",typeof<PLazy>,arg.GetType())
        
    override this.ToString() = 
        sprintf "eval ..."

