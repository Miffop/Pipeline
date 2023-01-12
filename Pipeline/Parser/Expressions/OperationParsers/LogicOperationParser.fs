namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions

type LogicOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"&"
            ->11
        |"|"
            ->12
        |"^"
            ->13
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"&"->LiteralExpression(AndFunc(),Some strImage)
        |"|"->LiteralExpression(OrFunc(),Some strImage)
        |"^"->LiteralExpression(XorFunc(),Some strImage)
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        ApplyExpression(ApplyExpression(this.Nullari(op,strImage),r,Some strImage),l,Some strImage)