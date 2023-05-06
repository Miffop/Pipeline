namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
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
        |"+"->Op(ALU.Add)
        |"-"->Op(ALU.Sub)
        |"*"->Op(ALU.Mul)
        |"/"->Op(ALU.Div)
        |"%"->Op(ALU.Mod)
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r,strImage) = 
        match op.Content with
        |"+"->Apply(F Function.I,r)
        |"-"->Apply(Op ALU.Neg,r)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.UnaryLeft(op,l,strImage) = 
        match op.Content with
        |"+"
        |"-"->raise<|OperationIsNotUnaryLeft(op.Content)
        |_->raise<|OperationIsNotUnary(op.Content)
    override this.Binary(op,l,r,strImage) = 
        Apply(Apply(this.Nullari(op,strImage),l),r)
