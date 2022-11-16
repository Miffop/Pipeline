namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type MarkerParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        if length = 3 && code.[index].Type = "Keyword" && code.[index].Content = "маркер" && code.[index+1].Type = "Word" && code.[index+2].Type = "Int" then
            let strImage =Some<|ep.CreateStringImage(code,index,length)
            let name = code.[index+1].Content
            let ecnt = code.[index+2].Content |> int
            Some<|MarkerDefExpression(name,ecnt,strImage)
        else
            None
            