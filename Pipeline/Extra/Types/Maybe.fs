namespace Pipeline.Extra.Types

open Pipeline.AST

type NoneImport() = 
    interface PipelineNamedImportable with
        member this.Name = "нет"
        member this.Import = Data<|Marker("нет",[])
type SomeImport() = 
    interface PipelineNamedImportable with
        member this.Name = "есть"
        member this.Import = Func<|MarkerFunc("есть",1)

type MaybeMonad() = 
    inherit PMonad()
    override this.Bind(a,f)=
        match a with
        |Data(:?Marker as m) when m.Name = "нет" && m.Elements.Length = 0 ->
                a
        |Data(:?Marker as m) when m.Name = "есть" && m.Elements.Length = 1 ->
                f.Eval(m.Elements.Head)
        |_->failwith "ожидался маркер"
    override this.Return = 
        {
            new PFunc() with
            member this.Eval(arg) =
                Data<|new Marker("есть",arg::[])
        }
    interface PipelineNamedImportable with
        member this.Name = "Возможность"
        member this.Import = Data<|MaybeMonad()