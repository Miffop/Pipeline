namespace Pipeline.AST.Funcs

open Pipeline.AST

type ForLoopFunc(toYield:bool) = 
    inherit SeparatorFunc()
    override this.EvalData(seq) =
        Func(ForLoopFuncCurried(toYield,seq))
    override this.ToString() = 
        sprintf "для ... в ... %s ..." (if toYield then "вернуть" else "выполнить")
and ForLoopFuncCurried(toYield:bool,seq:PData) = 
    inherit SeparatorFunc()
    override this.EvalFunc(code) =
        (seq:?>PSeq)
        |> Seq.map(fun x->code.Eval(x))
        |> (fun x->if toYield then Data(x) else Func(Identity()))
    override this.ToString() = 
        sprintf "для... в %A %s ..." (seq) (if toYield then "вернуть" else "выполнить")
