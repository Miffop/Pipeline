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