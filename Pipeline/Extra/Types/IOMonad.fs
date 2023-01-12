namespace Pipeline.Extra.Types

open Pipeline.AST

type IOMonad() = 
    inherit PMonad()
    override this.Return = 
        {
            new PFunc() with
            override this.Eval(data) =
                Func<|IOTypeContainer(data)
        }
    override this.Bind(a,f) = 
        match a with
        |Func(:?IOTypeBind as a)->
            Func<|a.Append(fun arg->
                match f.Eval(arg) with
                |Func(:?IOType as io)->io
                |_->failwith "Ожидался тип ввода/вывода";
            )
        |Func(:?IOType as a)->
            Func<|new IOTypeBind(a,(fun arg->
                match f.Eval(arg) with
                |Func(:?IOType as io)->io
                |x->failwith "Ожидался тип ввода/вывода";
            )::[])
        |_->failwithf "Ожидался тип ввода/вывода"
    interface PipelineNamedImportable with
        member this.Name = "ВводВывод"
        member this.Import = Data<|IOMonad()

and [<AbstractClass>] IOType() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?RealWorld as rw) ->
            Data(this.Perform rw)
    abstract member Perform:RealWorld->RealWorld*PFunOrData
    override this.ToString() = 
        sprintf "[ВводВывод]"
and IOTypeContainer(value:PFunOrData) = 
    inherit IOType()
    override this.Perform(rw) = 
        (rw,value)
and IOTypeBind(origin:IOType,bindList:(PFunOrData->IOType)list) = 
    inherit IOType()
    override this.Eval(arg) = 
        match arg with
        |Data(:?RealWorld as rw) -> 
            Data(this.Perform rw)
    member this.Append(f:PFunOrData->IOType) = 
        new IOTypeBind(origin,f::bindList)
    override this.Perform(rw:RealWorld) =
        List.foldBack(fun (f:PFunOrData->IOType) (rw:RealWorld,ma:IOType)->
            let nrw,a = ma.Perform rw
            (nrw,f a)
        ) bindList (rw,origin)
        |>(fun (rw,ma)->ma.Perform rw)
and RealWorld() =
    inherit PData()
    member this.Write(p:PFunOrData) = 
        printfn "%O" p
    member this.Read() = 
        Data<|System.Console.ReadLine()

type PrintFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|
        {
        new IOType() with
        override this.Perform(rw) = 
            rw.Write(arg);
            (rw,Func<|Identity())
        }
    override this.ToString() = 
        sprintf "вывод"
    interface PipelineNamedImportable with
        member this.Import = Func<|PrintFunc()
        member this.Name = "вывод"
type InputFunc() =
    inherit IOType()
    override this.Perform(rw) = 
        (rw,rw.Read())
    override this.ToString() =
        sprintf "ввод"
    interface PipelineNamedImportable with
        member this.Import = Func<|new InputFunc()
        member this.Name = "ввод"
