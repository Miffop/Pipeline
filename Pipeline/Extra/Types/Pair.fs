namespace Pipeline.Extra.Types

open Pipeline.AST

type PairImport() =
    interface PipelineNamedImportable with
        member this.Name = "пара"
        member this.Import = Func<|MarkerFunc("пара",2)

type PairFirstFunc() = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        |Data(:?Marker as m) when m.Name = "пара" && m.Elements.Length = 2->
            m.Elements.Tail.Head
        |_->failwith "ожидался маркер"
    interface PipelineNamedImportable with
        member this.Name = "первый"
        member this.Import = Func<|PairFirstFunc()
type PairSecondFunc() = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        |Data(:?Marker as m) when m.Name = "пара"->
            m.Elements.Head
        |_->failwith "ожидался маркер"
    interface PipelineNamedImportable with
        member this.Name = "второй"
        member this.Import = Func<|PairSecondFunc()
