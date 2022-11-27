namespace Pipeline.AST.Funcs

open Pipeline.AST

type ForLoopFunc(toYield:bool) = 
    inherit SeparatorFunc()
    override this.EvalFunc(f) =
        Func(ForLoopFuncCurried(toYield,f))
    override this.ToString() = 
        if toYield then
            sprintf "отобразить ... ..."
        else
            sprintf "перебрать ... ..."
and ForLoopFuncCurried(toYield:bool,f:PFunc) = 
    inherit SeparatorFunc()
    override this.EvalData(seq) =
        (seq:?>PSeq)
        |> Seq.map(fun x->f.Eval(x))
        |> (fun x->if toYield then Data(x) else Seq.iter ignore x; Func(Identity()))
    override this.ToString() = 
        if toYield then
            sprintf "отобразить (%O) ..." f
        else
            sprintf "перебрать (%O) ..." f
type ForYieldImport() = 
    interface PipelineNamedImportable with
        member this.Name = "отобразить"
        member this.Import = Func<|ForLoopFunc(true)
type ForDoImport() = 
    interface PipelineNamedImportable with
        member this.Name = "перебрать"
        member this.Import = Func<|ForLoopFunc(false)
