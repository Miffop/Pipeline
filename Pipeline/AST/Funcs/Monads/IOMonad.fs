namespace Pipeline.AST.Funcs.Monads

open Pipeline.AST

type IOMonad() = 
    inherit PMonad()
    override this.Return = 
        {
            new PFunc() with
            override this.Eval(data) =
                Func<|IOType(fun rw->(rw,data))
        }
    override this.Bind(a,f) = 
        match a with
        |Func(:?IOType as a)->
            Func<|IOType(fun rw->
                let nrw,res = a.Perform(rw)
                match f.Eval(res) with
                |Func(:?IOType as f)->
                    f.Perform(nrw)
                |_->failwith "Ожидался тип ввода/вывода"
            )
        |_->failwithf "Ожидался тип ввода/вывода"
    interface PipelineNamedImportable with
        member this.Name = "ВводВывод"
        member this.Import = Data<|IOMonad()
and IOType(operation:RealWorld->RealWorld*PFunOrData) =
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?RealWorld as rw) -> 
            Data(this.Perform rw)
    member this.Perform(rw:RealWorld) = operation rw
    override this.ToString() = 
        sprintf "[ВводВывод]"
and RealWorld() =
    inherit PData()
    member this.Write(p:PFunOrData) = 
        match p with
        |Data(d)->printfn "%O" d
        |Func(f)->printfn "%O" f
    member this.Read() = 
        Data<|System.Console.ReadLine()

type PrintFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|
        IOType(fun rw->
            rw.Write(arg)
            rw,Func<|Identity()
        )
    override this.ToString() = 
        sprintf "вывод"
    interface PipelineNamedImportable with
        member this.Import = Func<|PrintFunc()
        member this.Name = "вывод"
type InputFunc() =
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|
        IOType(fun rw->
            rw,rw.Read()
        )
    override this.ToString() =
        sprintf "ввод"
    interface PipelineNamedImportable with
        member this.Import = Func<|InputFunc()
        member this.Name = "ввод"
