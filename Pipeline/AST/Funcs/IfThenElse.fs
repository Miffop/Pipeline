namespace Pipeline.AST.Funcs

open Pipeline.AST


type CondFunc() = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        | Data(:?bool as c) -> Func<|CondFuncCurried(c)
        | Data(d)->raise <| WrongTypeException("if-then-else",typeof<bool>,d.GetType())
        | Func(f)->raise <| WrongTypeException("if-then-else",typeof<bool>,f.GetType())
    override this.ToString() = 
        sprintf "если ... то ... иначе ..."
    interface PipelineNamedImportable with
        member this.Import = Func<|CondFunc()
        member this.Name = "если"

and CondFuncCurried(c:bool) =
    inherit PFunc()
    override this.Eval(a) =
        match a with
        |Data(:?PLazy as a)->Func<|CondFuncCurriedCurried(c,a)
        |_->raise <| System.NotImplementedException()
    override this.ToString() = 
        sprintf "если %b то ... иначе ..." c
and CondFuncCurriedCurried(c:bool,a:PLazy) = 
    inherit PFunc()
    override this.Eval(b) =
        match b with
        |Data(:?PLazy as b)->if c then a.Value else b.Value
        |_->raise<|System.NotImplementedException()
    override this.ToString() = 
        sprintf "если %b то %O иначе ..." c a