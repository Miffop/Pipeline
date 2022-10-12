namespace Pipeline.AST


type CodeBlock(Commands:IExpression seq) = 
    interface IExpression with
        member this.Eval (c:PContex,object:obj) = 
            let contex=new PContex(Some c)
            Commands
            |>Seq.fold(fun a x->
                match x.Eval <| (contex,a) with
                |Fun(f)->
                    f.Invoke(a)
                |Data(d)->
                    d
            ) null
            |> Data
            