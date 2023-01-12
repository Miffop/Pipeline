namespace Pipeline.AST.Funcs

open Pipeline.AST



(*
 * Equal
 *)

type EqualFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,EqualPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,EqualPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,EqualPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,EqualPerformerBool())
    override this.CurryObj a =
        match a with
        | :?Marker as m->OperationExpressionCurried<Marker>(m,EqualPerformerMarker())
        | :?(PFunOrData list) as l-> OperationExpressionCurried<PFunOrData list>(l,EqualPerformerList())
        |_->raise<|System.NotImplementedException()
    interface PipelineNamedImportable with
        member this.Import = Func<|EqualFunc()
        member this.Name = "равно"
and EqualPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a=b)
    override this.ToString() = "(=)"
and EqualPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a=b)
    override this.ToString() = "(=)"
and EqualPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a=b)
    override this.ToString() = "(=)"
and EqualPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a=b)
    override this.ToString() = "(=)"
and EqualPerformerMarker() = 
    inherit IPerformer<Marker>()
    override this.Perform (a,b) = box(a.Name = b.Name && a.Elements.Length=b.Elements.Length && (a.Elements,b.Elements)||>List.forall2(fun x y->x=y))
    override this.ToString() = "(=)"
and EqualPerformerList() = 
    inherit IPerformer<PFunOrData list>()
    override this.Perform (a,b) = box(a.Length=b.Length && (a,b)||>List.forall2(fun x y->x=y))
    override this.ToString() = "(=)"


(*
 * NoEqual
 *)
type NotEqualFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,NotEqualPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,NotEqualPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,NotEqualPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,NotEqualPerformerBool())
    override this.CurryObj a =
        match a with
        | :?Marker as m->OperationExpressionCurried<Marker>(m,NoEqualPerformerMarker())
        | :?(PFunOrData list) as l-> OperationExpressionCurried<PFunOrData list>(l,NoEqualPerformerList())
        |_->raise<|System.NotImplementedException()
    interface PipelineNamedImportable with
        member this.Import = Func<|NotEqualFunc()
        member this.Name = "неравно"
and NotEqualPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a<>b)
    override this.ToString() = "(<>)"
and NotEqualPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a<>b)
    override this.ToString() = "(<>)"
and NotEqualPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a<>b)
    override this.ToString() = "(<>)"
and NotEqualPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a<>b)
    override this.ToString() = "(<>)"
and NoEqualPerformerMarker() = 
    inherit IPerformer<Marker>()
    override this.Perform (a,b) = box((a.Name = b.Name && a.Elements.Length=b.Elements.Length && (a.Elements,b.Elements)||>List.forall2(fun x y->x=y))|>not)
    override this.ToString() = "(<>)"
and NoEqualPerformerList() = 
    inherit IPerformer<PFunOrData list>()
    override this.Perform (a,b) = box((a.Length=b.Length && (a,b)||>List.forall2(fun x y->x=y))|>not)
    override this.ToString() = "(<>)"


(*
 * Greater
 *)
type GreaterFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,GreaterPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,GreaterPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,GreaterPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,GreaterPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|GreaterFunc()
        member this.Name = "больше"
and GreaterPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a>b)
    override this.ToString() = "(>)"
and GreaterPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a>b)
    override this.ToString() = "(>)"
and GreaterPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a>b)
    override this.ToString() = "(>)"
and GreaterPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a>b)
    override this.ToString() = "(>)"

(*
 * Less
 *)
type LessFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,LessPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,LessPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,LessPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,LessPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|LessFunc()
        member this.Name = "меньше"
and LessPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a<b)
    override this.ToString() = "(<)"
and LessPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a<b)
    override this.ToString() = "(<)"
and LessPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a<b)
    override this.ToString() = "(<)"
and LessPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a<b)
    override this.ToString() = "(<)"

(*
 * GreaterEqual
 *)
type GreaterEqualFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,GreaterEqualPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,GreaterEqualPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,GreaterEqualPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,GreaterEqualPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|GreaterEqualFunc()
        member this.Name = "большеРавно"
and GreaterEqualPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a>=b)
    override this.ToString() = "(>=)"
and GreaterEqualPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a>=b)
    override this.ToString() = "(>=)"
and GreaterEqualPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a>=b)
    override this.ToString() = "(>=)"
and GreaterEqualPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a>=b)
    override this.ToString() = "(>=)"

(*
 * LessEqual
 *)
type LessEqualFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,LessEqualPerformerInt())
    override this.CurryFloat a = 
        OperationExpressionCurried<float>(a,LessEqualPerformerFloat())
    override this.CurryString a = 
        OperationExpressionCurried<string>(a,LessEqualPerformerString())
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,LessEqualPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|LessEqualFunc()
        member this.Name = "меньшеРавно"
and LessEqualPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a<=b)
    override this.ToString() = "(<=)"
and LessEqualPerformerFloat() = 
    inherit IPerformer<float>()
    override this.Perform (a,b) = box(a<=b)
    override this.ToString() = "(<=)"
and LessEqualPerformerString() = 
    inherit IPerformer<string>()
    override this.Perform (a,b) = box(a<=b)
    override this.ToString() = "(<=)"
and LessEqualPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a<=b)
    override this.ToString() = "(<=)"