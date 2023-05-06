namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type DefAndFuncParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        if length > 2 && code.[index].Type = "Keyword" && code.[index+1].Type = "Word" then 
            let strImage =Some<|ep.CreateStringImage(code,index,length)
            match code.[index].Content with
            |"от"->Some<|Lambda(code[index+1].Content,ep.ParseExpression(code,index+2,length-2))
            //|"пусть"->Some<|DefExpression(code[index+1].Content,ContextIsolationExpression(ep.ParseExpression(code,index+2,length-2)),strImage)
            |_->None
        else
            None