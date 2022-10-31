namespace Pipeline.AST.Expressions.Constructs

open Pipeline.AST

type ForLoopFunc(toYield:bool) = 
    inherit SeparatorFunc()
    override this.EvalData(seq) =
        Func(ForLoopFuncCurried(toYield,seq))
and ForLoopFuncCurried(toYield:bool,seq:PData) = 
    inherit SeparatorFunc()
    override this.EvalFunc(code) =
        (seq:?>System.Collections.IEnumerable).Seq
        |> Seq.map(function | :?PFunc as f -> Func(f) |d -> Data(d))
        |> Seq.map(fun x->code.Eval(x))
        |> (fun x->if toYield then Data(x) else Func(Identity()))