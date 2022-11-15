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
    override this.Nullari(op) = 
        match op.Content with
        |"+"->LiteralExpression(SumFunc())
        |"-"->LiteralExpression(DiffFunc())
        |"*"->LiteralExpression(MulFunc())
        |"/"->LiteralExpression(DivFunc())
        |"%"->LiteralExpression(ModFunc())
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r) = 
        match op.Content with
        |"+"->ApplyExpression(LiteralExpression(Identity()),r)
        |"-"->ApplyExpression(LiteralExpression(NegFunc()),r)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.UnaryLeft(op,l) = 
        match op.Content with
        |"+"
        |"-"->raise<|OperationIsNotUnaryLeft(op.Content)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.Binary(op,l,r) = 
        ApplyExpression(ApplyExpression(this.Nullari(op),r),l)
