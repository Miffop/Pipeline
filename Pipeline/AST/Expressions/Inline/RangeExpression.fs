namespace Pipeline.AST.Expressions.Inline

open Pipeline.AST

type RangeExpression(aExp:IExpression,bExp:IExpression,dExp:IExpression option) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            let d = match dExp with|Some(dExp)->Some(dExp.Eval (c,o)) |None->None
            match a,b,d with
            |Data(:?int as a),Data(:?int as b),None->
                Data(seq{a..b})
            |Data(:?int as a),Data(:?int as b),Some(Data(:?int as d))->
                Data(seq{a..d..b})


