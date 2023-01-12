namespace Pipeline.AST.Funcs

open Pipeline.AST

type ForLoopFunc() = 
    inherit SeparatorFunc()
    override this.EvalFunc(f) =
        Func(ForLoopFuncCurried(f))
    override this.ToString() = 
        sprintf "отобразить ... ..."
    interface PipelineNamedImportable with
        member this.Name = "отобразить"
        member this.Import = Func<|ForLoopFunc()
and ForLoopFuncCurried(f:PFunc) = 
    inherit SeparatorFunc()
    override this.EvalData(seq) =
        (seq:?>PList)
        |> List.map(fun x->f.Eval(x))
        |> (fun x->Data(x))
    override this.ToString() = 
        sprintf "отобразить (%O) ..." f
