namespace Pipeline.AST


type CodeBlock(Commands:IExpression seq) = 
    interface IExpression with
        member this.Eval (c:PContex) = 
            let contex=new PContex(Some c)
            Commands
            |>Seq.fold(fun a x->
                match x.Eval contex with
                |Fun(f)->
                    f.Invoke(a)
                |Data(d)->
                    if a=null then
                        d
                    else
                        raise <| new System.Exception "Cannot change the object"
            ) contex.Object
            |> Data
            