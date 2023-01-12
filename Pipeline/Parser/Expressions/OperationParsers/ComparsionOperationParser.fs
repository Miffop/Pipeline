namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions

type ComparisonOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"?"
        |"="
        |"!="
        |">"
        |">="
        |"<"
        |"<="
            ->10
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"?"->LiteralExpression(MarkerCheckFunc(),Some strImage)
        |"="->LiteralExpression(EqualFunc(),Some strImage)
        |"!="->LiteralExpression(NotEqualFunc(),Some strImage)
        |">"->LiteralExpression(GreaterFunc(),Some strImage)
        |"<"->LiteralExpression(LessFunc(),Some strImage)
        |">="->LiteralExpression(GreaterEqualFunc(),Some strImage)
        |"<="->LiteralExpression(LessEqualFunc(),Some strImage)
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        ApplyExpression(ApplyExpression(this.Nullari(op,strImage),r,Some strImage),l,Some strImage)