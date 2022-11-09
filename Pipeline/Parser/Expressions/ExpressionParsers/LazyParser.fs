namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type LazyParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        let first = Seq.item index code
        if first.Type = "Keyword" && first.Content = "lazy" then
            Some<|LazyExpression(ep.ParseExpression(code,index+1,length-1))
        else
            None