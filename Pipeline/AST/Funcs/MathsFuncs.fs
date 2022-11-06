namespace Pipeline.AST.Funcs

open Pipeline.AST



[<AbstractClass>]
type DataOperationFunc() = 
    inherit SeparatorFunc()
    override this.EvalData(arg) =
        match arg with
        | :? int as a -> this.CurryInt a
        | :? float as a -> this.CurryFloat a
        | :? string as a -> this.CurryString a
        | a -> this.CurryObj a
        |> Func
    abstract CurryInt:int->PFunc
    abstract CurryFloat:float->PFunc
    abstract CurryString:string->PFunc
    abstract CurryObj:obj->PFunc
    default this.CurryObj a =
        raise <| System.NotImplementedException()
[<AbstractClass>]
type IPerformer<'T>() = 
    abstract Perform:'T*'T->PData
type OperationExpressionCurried<'T>(a:'T,p:IPerformer<'T>) = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :?'T as b -> Data(p.Perform(b,a))
        | b -> raise <| TypeMatchException(Data a,Data b)
    override this.ToString() = 
        sprintf"%s %O ..." <| p.ToString() <| a
    


type SumFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,SumPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,SumPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,SumPerformerString())
and SumPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a+b)
    override this.ToString() = "(+)"
and SumPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a+b)
    override this.ToString() = "(+)"
and SumPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a+b)
    override this.ToString() = "(+)"

type DiffFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,DiffPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,DiffPerformerFloat())
    override this.CurryString a = 
        raise<|System.NotImplementedException()
and DiffPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a-b)
    override this.ToString() = "(-)"
and DiffPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a-b)
    override this.ToString() = "(-)"
type NegFunc() = 
    inherit SeparatorFunc()
    override this.EvalData(arg) = 
        match arg with
        | :? int as a->Data(-a)
        | :? float as a->Data(-a)
        |_->raise <| WrongTypeException("-",typeof<int>,arg.GetType())

type MulFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,MulPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,MulPerformerFloat())
    override this.CurryString a = 
        raise<|System.NotImplementedException()
and MulPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a*b)
    override this.ToString() = "(*)"
and MulPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a*b)
    override this.ToString() = "(*)"

type DivFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,DivPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,DivPerformerFloat())
    override this.CurryString a = 
        raise<|System.NotImplementedException()
and DivPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a/b)
    override this.ToString() = "(/)"
and DivPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a/b)
    override this.ToString() = "(/)"

type ModFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,ModPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,ModPerformerFloat())
    override this.CurryString a = 
        raise<|System.NotImplementedException()
and ModPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(if sign(a)=sign(b) then a%b else a%b+b)
    override this.ToString() = "(%)"
and ModPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(if sign(a)=sign(b) then a%b else a%b+b)
    override this.ToString() = "(%)"

