namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type LiteralParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        if length = 1 then
            let tok = (Seq.item index code)
            let strImage =Some<|ep.CreateStringImage(code,index,length)
            match tok.Type with
            |"String" ->Some<|String(tok.Content)
            |"Int" ->Some<|Int(tok.Content |> System.Int32.Parse)
            |"Float" ->Some<|Float(tok.Content|>float)
            |_->None
            |>Option.map L
        else
            None