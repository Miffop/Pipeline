namespace Pipeline.AST.Expressions.Commands

open Pipeline.AST

type IntExpression() =
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToInt32 arg)
    override this.ToString() = "int ..."
type FloatExpression() = 
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToDouble arg)
    override this.ToString() = "float ..."
type StringExpression() = 
    inherit SeparatorFunc()
    override this.EvalData (arg) = 
        Data(System.Convert.ToString arg)
    override this.EvalFunc (arg) = 
        Data(arg.ToString())
    override this.ToString() = "string ..."