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
            let strImage =Some<|ep.CreateStringImage(code,index,length)
            match first.Content with
            |"ленивое" -> Some<|LazyExpression(ep.ParseExpression(code,index+1,length-1),strImage)
            |"lazyBlock" -> Some<|LazyExpression(ContextIsolationExpression(ep.ParseExpression(code,index+1,length-1)),strImage) 
            |_->None
        else
            None