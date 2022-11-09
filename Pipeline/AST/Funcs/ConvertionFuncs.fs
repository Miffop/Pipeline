namespace Pipeline.AST.Funcs

open Pipeline.AST

type IntFunc() =
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToInt32 arg)
    override this.ToString() = "int ..."
    interface PipelineNamedImportable with
        member this.Import = Func<|IntFunc()
        member this.Name = "целое"
type FloatFunc() = 
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToDouble arg)
    override this.ToString() = "float ..."
    interface PipelineNamedImportable with
        member this.Import = Func<|FloatFunc()
        member this.Name = "дробь"
type StringFunc() = 
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToString arg)
    override this.EvalFunc (arg) = 
        Data(arg.ToString())
    override this.ToString() = "string ..."
    interface PipelineNamedImportable with
        member this.Import = Func<|StringFunc()
        member this.Name = "строка"