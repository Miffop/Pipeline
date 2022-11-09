namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type LazyParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        let first = Seq.item index code
        if first.Type = "Keyword" then
            match first.Content with
            |"отложить" -> Some<|LazyExpression(ep.ParseExpression(code,index+1,length-1))
            |"lazyBlock" -> Some<|LazyExpression(ContextIsolationExpression(ep.ParseExpression(code,index+1,length-1))) 
            |_->None
        else
            None