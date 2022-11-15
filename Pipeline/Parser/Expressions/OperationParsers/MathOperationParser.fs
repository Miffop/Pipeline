namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

type MathOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op:Token) = 
        match op.Content with
        |"+"|"-"->2
        |"*"|"/"|"%"->1
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"+"->LiteralExpression(SumFunc(),Some strImage)
        |"-"->LiteralExpression(DiffFunc(),Some strImage)
        |"*"->LiteralExpression(MulFunc(),Some strImage)
        |"/"->LiteralExpression(DivFunc(),Some strImage)
        |"%"->LiteralExpression(ModFunc(),Some strImage)
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r,strImage) = 
        match op.Content with
        |"+"->ApplyExpression(LiteralExpression(Identity(),Some strImage),r,Some strImage)
        |"-"->ApplyExpression(LiteralExpression(NegFunc(),Some strImage),r,Some strImage)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.UnaryLeft(op,l,strImage) = 
        match op.Content with
        |"+"
        |"-"->raise<|OperationIsNotUnaryLeft(op.Content)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.Binary(op,l,r,strImage) = 
        ApplyExpression(ApplyExpression(this.Nullari(op,strImage),r,Some strImage),l,Some strImage)
