namespace Pipeline.Extra.Funcs

open Pipeline.AST

type SeqInit() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?int as cnt)->Func<|SeqInitCurried(cnt)
        |_->failwith "ожидалось число"
    interface PipelineNamedImportable with
        member this.Name = "создать"
        member this.Import = Func<|SeqInit()
and SeqInitCurried(cnt:int) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            Data<|Seq.init cnt (fun x->f.Eval(Data x))
        |_->failwith "ожидалось число"
type SeqFold() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            let folder s x= 
                match f.Eval(s) with
                |Func(fs)->fs.Eval(x)
                |_->failwith "ожидалась функция"
            Func<|SeqFoldCurried(folder)
        |_->failwith "ожидалась функция"
    interface PipelineNamedImportable with
        member this.Name = "свернуть"
        member this.Import = Func<|SeqFold()
and SeqFoldCurried(folder:PFunOrData->PFunOrData->PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|SeqFoldCurriedCurried(folder,arg)
and SeqFoldCurriedCurried(folder,first:PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PSeq as seq)->
            seq|>Seq.fold(folder)first
        |_->failwith "ожидалась последовательность"