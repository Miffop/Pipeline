namespace Pipeline.AST.Funcs

open Pipeline.AST

type RangeFunc() =
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as a -> Func(RangeFuncCurried(a))
        |_->raise <| WrongTypeException("Range expression",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "... to ..."

and RangeFuncCurried(a:int) = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?int as b -> Data(seq{a..b})
        |_->raise <| WrongTypeException("Range expression",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "%i to ..." a

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
        | :?int as d -> Data(seq{a..d..b})
        |_->raise <| WrongTypeException("Range expression with delta",typeof<int>,arg.GetType())
    override this.ToString() = 
        sprintf "%i to %i with step of ..." a b