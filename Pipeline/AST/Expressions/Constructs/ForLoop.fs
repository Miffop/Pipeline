namespace Pipeline.AST.Expressions.Constructs

open Pipeline.AST

type ForExpression(elemName:string,indexNameOption:string option,boxedSeqExpOption:IExpression option,toYield:bool,code:IExpression) =
    interface IExpression with
        member this.Eval (c,o) =
            let boxedSeq = 
                match boxedSeqExpOption with
                |Some(boxedSeqExp)->
                    match boxedSeqExp.Eval (c,o) with
                    |Data(boxedSeq)->boxedSeq
                    |wrongObject->raise<|NoSeqException("for loop",wrongObject)
                |None->o
            let objectSeq = 
                match boxedSeq with
                | :? PSeq as x->
                    x.Seq
                |wrongObject->raise<|NoSeqException("for loop",wrongObject)
            objectSeq
            |>Seq.mapi(fun i x->
                let contex = PContex(Some c)
                contex.Def elemName (Data x)
                match indexNameOption with
                |Some(indexName)->contex.Def indexName (Data i)
                |_->()
                match code.Eval(contex,null) with
                |Data(d)->d
                |_ when toYield->raise<|SeqOfFunctionException()
                |_->null
            )
            |>(fun x->Data(if toYield then x else Seq.iter(fun x->())x;null))

