namespace Pipeline.AST.Funcs

open Pipeline.AST

type ForLoopFunc(toYield:bool) = 
    inherit SeparatorFunc()
    override this.EvalData(seq) =
        Func(ForLoopFuncCurried(toYield,seq))
    override this.ToString() = 
        sprintf "for ... in ... %s ..." (if toYield then "yield" else "do")
and ForLoopFuncCurried(toYield:bool,seq:PData) = 
    inherit SeparatorFunc()
    override this.EvalFunc(code) =
        (seq:?>System.Collections.IEnumerable).Seq
        |> Seq.map(function | :?PFunc as f -> Func(f) |d -> Data(d))
        |> Seq.map(fun x->code.Eval(x))
        |> (fun x->if toYield then Data(x) else Func(Identity()))
    override this.ToString() = 
        sprintf "for ... in %A %s ..." (seq) (if toYield then "yield" else "do")
