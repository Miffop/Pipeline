namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.Extra.Types
open Pipeline.AST.Expressions

type ExtraOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |","
            ->15
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |","->LiteralExpression(ListMarkerFunc(),Some strImage)
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        ApplyExpression(ApplyExpression(this.Nullari(op,strImage),l,Some strImage),r,Some strImage)
    override this.GetOrderDirection op= 
        match op.Content with
        |","
            ->false
        |_->raise<|System.NotImplementedException()