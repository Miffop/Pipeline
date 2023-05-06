namespace Pipeline.Parser.Expressions.ExpressionParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions


type BraceBreakParser() = 
    inherit IExpressionParser()
    override this.GetExpression(code,index,length,ep) = 
        let first = (Seq.item (index) code)
        let last = (Seq.item (index+length-1) code)
        if 
            first.Type = "BraceOpen" &&
            last.Type = "BraceClose"
        then
            let braceCounter = ref 1
            let len = ref 1
            while !braceCounter>0 do
                match (Seq.item (index+ !len) code).Type with
                |"BraceClose"->decr braceCounter
                |"BraceOpen"->incr braceCounter
                |_->()
                incr len
            if !len = length then
                Some<|ep.ParseExpression(code,index+1,length-2)
            else
                None
        else
            None