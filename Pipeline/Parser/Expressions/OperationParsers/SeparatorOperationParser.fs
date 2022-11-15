namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

[<AbstractClass>]
type SeparatorOperationParser() = 
    inherit IOperationExpressionParser()
    override this.GetExpression(op,left,right)  =
        match left,right with
        | :? EmptyExpression as _, :? EmptyExpression as _ -> this.Nullari(op)
        | l, :? EmptyExpression as _ -> this.UnaryLeft(op,l)
        | :? EmptyExpression as _,r -> this.UnaryRight(op,r)
        | l,r -> this.Binary(op,l,r)
    abstract Nullari:op:Token->IExpression
    abstract UnaryLeft:op:Token*left:IExpression->IExpression
    abstract UnaryRight:op:Token*right:IExpression->IExpression
    abstract Binary:op:Token*left:IExpression*right:IExpression->IExpression

    
    default this.Nullari (op) = raise<|System.NotImplementedException()
    default this.UnaryLeft (op,l) = raise<|System.NotImplementedException()
    default this.UnaryRight (op,r) = raise<|System.NotImplementedException()
    default this.Binary (op,l,r) = raise<|System.NotImplementedException()