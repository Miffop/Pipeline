namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

[<AbstractClass>]
type SeparatorOperationParser() = 
    inherit IOperationExpressionParser()
    override this.GetExpression(op,left,right,strImage)  =
        match left,right with
        | NullExpression, NullExpression -> this.Nullari(op,strImage)
        | l, NullExpression -> this.UnaryLeft(op,l,strImage)
        | NullExpression as _,r -> this.UnaryRight(op,r,strImage)
        | l,r -> this.Binary(op,l,r,strImage)
    abstract Nullari:op:Token*strImage:StringImage->Expression
    abstract UnaryLeft:op:Token*left:Expression*strImage:StringImage->Expression
    abstract UnaryRight:op:Token*right:Expression*strImage:StringImage->Expression
    abstract Binary:op:Token*left:Expression*right:Expression*strImage:StringImage->Expression

    
    default this.Nullari (op,sI) = raise<|System.NotImplementedException()
    default this.UnaryLeft (op,l,sI) = raise<|System.NotImplementedException()
    default this.UnaryRight (op,r,sI) = raise<|System.NotImplementedException()
    default this.Binary (op,l,r,sI) = raise<|System.NotImplementedException()