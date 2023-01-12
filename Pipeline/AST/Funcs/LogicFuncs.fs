namespace Pipeline.AST.Funcs

open Pipeline.AST;

(*
 * And
 *)
type AndFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,AndPerformerInt())
    override this.CurryFloat a = 
        raise<|System.NotImplementedException()
    override this.CurryString a = 
        raise<|System.NotImplementedException()
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,AndPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|AndFunc()
        member this.Name = "и"
and AndPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a &&& b)
    override this.ToString() = "(&)"
and AndPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a && b)
    override this.ToString() = "(&)"

(*
 * Or
 *)
type OrFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,OrPerformerInt())
    override this.CurryFloat a = 
        raise<|System.NotImplementedException()
    override this.CurryString a = 
        raise<|System.NotImplementedException()
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,OrPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|OrFunc()
        member this.Name = "или"
and OrPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a ||| b)
    override this.ToString() = "(|)"
and OrPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a || b)
    override this.ToString() = "(|)"

(*
 * Xor
 *)
type XorFunc() = 
    inherit DataOperationFunc()
    override this.CurryInt a = 
        OperationExpressionCurried<int>(a,XorPerformerInt())
    override this.CurryFloat a = 
        raise<|System.NotImplementedException()
    override this.CurryString a = 
        raise<|System.NotImplementedException()
    override this.CurryBool a = 
        OperationExpressionCurried<bool>(a,XorPerformerBool())
    interface PipelineNamedImportable with
        member this.Import = Func<|XorFunc()
        member this.Name = "исключающееИли"
and XorPerformerInt() = 
    inherit IPerformer<int>()
    override this.Perform (a,b) = box(a ^^^ b)
    override this.ToString() = "(^)"
and XorPerformerBool() = 
    inherit IPerformer<bool>()
    override this.Perform (a,b) = box(a <> b)
    override this.ToString() = "(^)"


type NotFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?int as i)->Data(~~~i)
        |Data(:?bool as b)->Data(not b)
        |_->raise<|System.NotImplementedException()
    override this.ToString() = "не"
    interface PipelineNamedImportable with
        member this.Import = Func<|NotFunc()
        member this.Name = "не"
