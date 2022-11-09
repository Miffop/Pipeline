namespace Pipeline.AST.Funcs

open Pipeline.AST


type EvalLazyFunc() = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        | Data(:?PLazy as l) ->l.Value
        |_->raise<|WrongTypeException("вычислить",typeof<PLazy>,arg.GetType())        
    override this.ToString() = 
        sprintf "вычислить ..."
    interface PipelineNamedImportable with
        member this.Import = Func <| EvalLazyFunc()
        member this.Name = "вычислить"
