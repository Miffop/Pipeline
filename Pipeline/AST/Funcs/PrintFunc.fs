namespace Pipeline.AST.Funcs

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
        sprintf "вывести"
    interface PipelineNamedImportable with
        member this.Import = Func<|PrintFunc()
        member this.Name = "вывести"