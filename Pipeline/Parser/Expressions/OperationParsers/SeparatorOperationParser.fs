namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

[<AbstractClass>]
type SeparatorOperationParser() = 
    inherit IOperationExpressionParser()
    override this.GetExpression(op,left,right,strImage)  =
        match left,right with
        | :? EmptyExpression as _, :? EmptyExpression as _ -> this.Nullari(op,strImage)
        | l, :? EmptyExpression as _ -> this.UnaryLeft(op,l,strImage)
        | :? EmptyExpression as _,r -> this.UnaryRight(op,r,strImage)
        | l,r -> this.Binary(op,l,r,strImage)
    abstract Nullari:op:Token*strImage:StringImage->IExpression
    abstract UnaryLeft:op:Token*left:IExpression*strImage:StringImage->IExpression
    abstract UnaryRight:op:Token*right:IExpression*strImage:StringImage->IExpression
    abstract Binary:op:Token*left:IExpression*right:IExpression*strImage:StringImage->IExpression

    
    default this.Nullari (op,sI) = raise<|System.NotImplementedException()
    default this.UnaryLeft (op,l,sI) = raise<|System.NotImplementedException()
    default this.UnaryRight (op,r,sI) = raise<|System.NotImplementedException()
    default this.Binary (op,l,r,sI) = raise<|System.NotImplementedException()