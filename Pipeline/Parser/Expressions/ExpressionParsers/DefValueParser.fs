namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type DefValueParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        if length = 1 then
            let tok = (Seq.item index code)
            match tok.Type with
            |"Word" ->Some<|DefValueExpression(tok.Content)
            |_->None
        else
            None