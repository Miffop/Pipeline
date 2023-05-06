namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.Parser.Expressions

(*
type MonadParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        if code.[index].Type = "Keyword" && code.[index+1].Type = "Word" then
            let strImage = ep.CreateStringImage(code,index,length)
            match code.[index].Content with
            |"монада"->Some<|MonadDefExpression(code.[index+1].Content,ep.ParseExpression(code,index+2,length-2),Some strImage)
            |"применить"->Some<|MonadUseExpression(code.[index+1].Content,ep.ParseExpression(code,index+2,length-2),Some strImage)
            |_->None
        else
            None

*)