namespace Pipeline.AST.Expressions.Commands

open Pipeline.AST

type PrintFunc() = 
    inherit SeparatorFunc()
    override this.EvalData(d) =
        printfn "%O" d
        Func <| Identity()
    override this.EvalFunc(f) = 
        printfn "%s" <| f.ToString()
        Func <| Identity()
    override this.ToString() = 
        sprintf "print"