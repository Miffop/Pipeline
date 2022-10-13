namespace Pipeline.AST.Expressions.Commands

open Pipeline.AST

type IntExpression(exp:IExpression option) =
    new(exp)=new IntExpression(Some exp)
    interface IExpression with
        member this.Eval (c,o) = 
            let object:obj = 
                match exp with
                |Some(exp)->
                    let a = exp.Eval(c,o)
                    match a with
                    |Data(d)->d
                    |a->raise<|CannotPerformException("int",a)
                |None->
                    o
            Data(System.Convert.ToInt32 object)
type FloatExpression(exp:IExpression option) = 
    new(exp)=new FloatExpression(Some exp)
    interface IExpression with
        member this.Eval (c,o) = 
            let object:obj = 
                match exp with
                |Some(exp)->
                    let a = exp.Eval(c,o)
                    match a with
                    |Data(d)->d
                    |a->raise<|CannotPerformException("float",a)
                |None->
                    o
            Data(System.Convert.ToDouble object)
type StringExpression(exp:IExpression option) = 
    new(exp)=new StringExpression(Some exp)
    interface IExpression with
        member this.Eval (c,o) = 
            let object:obj = 
                match exp with
                |Some(exp)->
                    let a = exp.Eval(c,o)
                    match a with
                    |Data(d)->d
                    |a->raise<|CannotPerformException("string",a)
                |None->
                    o
            Data(System.Convert.ToString object)
