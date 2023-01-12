namespace Pipeline.AST.Funcs

open Pipeline.AST

type RangeFunc() =
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as a -> Func(RangeFuncCurried(a))
        |_->raise <| WrongTypeException("отрезок",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "отрезок ... ..."
    interface PipelineNamedImportable with
        member this.Name = "отрезок"
        member this.Import = Func<|RangeFunc()
and RangeFuncCurried(a:int) = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as b -> Data([a..b] |> List.map(fun x->Data x))
        |_->raise <| WrongTypeException("отрезок",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "отрезок %i ..." a

type RangeFuncWithDelta() =
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as a -> Func(RangeFuncWithDeltaCurried(a))
        |_->raise <| WrongTypeException("Range expression with delta",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "... to ... with step of ..."

and RangeFuncWithDeltaCurried(a:int) = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as b -> Func(RangeFuncWithDeltaCurriedCurried(a,b))
        |_->raise <| WrongTypeException("Range expression with delta",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "%i to ... with step of ..." a

and RangeFuncWithDeltaCurriedCurried(a:int,b:int) = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as d -> Data(seq{a..d..b} |> Seq.map(fun x->Data x))
        |_->raise <| WrongTypeException("Range expression with delta",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "%i to %i with step of ..." a b