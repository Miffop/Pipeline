namespace Pipeline.AST.Expressions.Inline

open Pipeline.AST


type TypeMatchExeption(a:PFunOrData,b:PFunOrData) = 
    inherit System.Exception()
    override this.Message = sprintf "types don't match: %A, %A" a b


type SumExpression(aExp:IExpression,bExp:IExpression) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            match a,b with
            |Data(:?int as a),Data(:?int as b)->
                Data(a+b)
            |Data(:?float as a),Data(:?float as b)->
                Data(a+b)
            |Data(:?string as a),Data(:?string as b)->
                Data(a+b)
            |_->raise <| new TypeMatchExeption(a,b)
type DiffExpression(aExp:IExpression,bExp:IExpression) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            match a,b with
            |Data(:?int as a),Data(:?int as b)->
                Data(a-b)
            |Data(:?float as a),Data(:?float as b)->
                Data(a-b)
            |_->raise <| new TypeMatchExeption(a,b)
type MulExpression(aExp:IExpression,bExp:IExpression) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            match a,b with
            |Data(:?int as a),Data(:?int as b)->
                Data(a*b)
            |Data(:?float as a),Data(:?float as b)->
                Data(a*b)
            |_->raise <| new TypeMatchExeption(a,b)
type DivExpression(aExp:IExpression,bExp:IExpression) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            match a,b with
            |Data(:?int as a),Data(:?int as b)->
                Data(a/b)
            |Data(:?float as a),Data(:?float as b)->
                Data(a/b)
            |_->raise <| new TypeMatchExeption(a,b)
type ModExpression(aExp:IExpression,bExp:IExpression) = 
    interface IExpression with
        member this.Eval (c,o) = 
            let a,b = aExp.Eval (c,o),bExp.Eval (c,o)
            match a,b with
            |Data(:?int as a),Data(:?int as b)->
                Data(if sign(a)=sign(b) then a%b else a%b+b)
            |Data(:?float as a),Data(:?float as b)->
                Data(if sign(a)=sign(b) then a%b else a%b+b)
            |_->raise <| new TypeMatchExeption(a,b)