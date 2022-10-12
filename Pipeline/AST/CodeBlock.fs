namespace Pipeline.AST


type CodeBlock(Commands:IExpression seq) = 
    interface IExpression with
        member this.Eval (c:PContex) = 
            let contex=new PContex(Some c)
            Commands
            |>Seq.fold(fun a x->
                contex.Object<-a
                match x.Eval contex with
                |Fun(f)->
                    f.Invoke(a)
                |Data(d)->
                    d
            ) contex.Object
            |> Data
            