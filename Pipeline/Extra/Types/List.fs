namespace Pipeline.Extra.Types

open Pipeline.AST


type ListMonad() =
    inherit PMonad()
    override this.Bind(a,f) = 
        match a with
        |Data(:?PList as l)->
            l
            |>List.collect(fun x->
                match f.Eval(x) with
                |Data(:?PList as l)->l
                |_->failwith "ожидался список"
            )
            |>(fun x->Data x)
        |_->failwith "ожидался список"
    override this.Return = 
        {
            new PFunc() with
            member this.Eval(arg) = 
                Data<|arg::[]
        }
    interface PipelineNamedImportable with
        member this.Name = "Список"
        member this.Import = Data<|ListMonad()

//--------------------
//======Essetial======
//--------------------
type ListMarkerFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|ListMarkerFuncCurried(arg)
and ListMarkerFuncCurried(head:PFunOrData) =
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            Data<|head::l
        |_->failwith "ожидался список"
type ListHeadFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            l.Head
        |_->failwith "ожидался список"
    interface PipelineNamedImportable with
        member this.Name = "голова"
        member this.Import = Func<|ListHeadFunc()
type ListTailFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            Data l.Tail
        |_->failwith "ожидался список"
    interface PipelineNamedImportable with
        member this.Name = "хвост"
        member this.Import = Func<|ListTailFunc()
type EmptyListImport() = 
    interface PipelineNamedImportable with
        member this.Name = "пустойСписок"
        member this.Import = Data<|(List.empty<PFunOrData>)
type ListInitFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?int as cnt)->Func<|ListInitFuncCurried(cnt)
        |_->failwith "ожидалось число"
    interface PipelineNamedImportable with
        member this.Name = "создать"
        member this.Import = Func<|ListInitFunc()
and ListInitFuncCurried(cnt:int) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            Data<|List.init cnt (fun x->f.Eval(Data x))
        |_->failwith "ожидалось число"

//------------------------
//======Transformers======
//------------------------
type ListFoldFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            let folder s x= 
                match f.Eval(s) with
                |Func(fs)->fs.Eval(x)
                |_->failwith "ожидалась функция"
            Func<|ListFoldFuncCurried(folder)
        |_->failwith "ожидалась функция"
    interface PipelineNamedImportable with
        member this.Name = "свернуть"
        member this.Import = Func<|ListFoldFunc()
and ListFoldFuncCurried(folder:PFunOrData->PFunOrData->PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        Func<|ListFoldFuncCurriedCurried(folder,arg)
and ListFoldFuncCurriedCurried(folder,first:PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as seq)->
            seq|>List.fold(folder)first
        |_->failwith "ожидалcя список"
type ListCollectFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            Func<|ListCollectFuncCurried(f)
        |_->failwith "ожидалась функция"
    interface PipelineNamedImportable with
        member this.Name = "собрать"
        member this.Import = Func<|ListCollectFunc()
and ListCollectFuncCurried(f:PFunc) =
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        |Data(:?PList as l)->
            l
            |>List.collect(fun x->
                match f.Eval(x) with
                |Data(:?PList as l)->l
                |_->failwith "ожидался список"
            )
            |>(fun x->Data x)
        |_->failwith "ожидался список"
type ListPairwiseFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            Data(l|>List.pairwise|>List.map(fun (a,b)->Data<|Marker("пара",b::a::[])))
        |_->failwithf "ожидался список"
    interface PipelineNamedImportable with
        member this.Name = "попарно"
        member this.Import = Func<|ListPairwiseFunc()
type ListReverseFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            Data(List.rev l)
        |_->failwithf "ожидался список"
    interface PipelineNamedImportable with
        member this.Name = "развернуть"
        member this.Import = Func<|ListReverseFunc()

//---------------------
//======Predicate======
//---------------------

type ListPredicateFunc(performer:(PFunOrData->bool)->PList->PFunOrData) = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Func(f)->
            Func<|ListPredicateFuncCurried(performer,f)
        |_->failwith "ожидалась функция"
and ListPredicateFuncCurried(performer,f:PFunc) =
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?PList as l)->
            l
            |>performer(fun x->
                match f.Eval(x) with
                |Data(:?bool as b)->b
                |_->failwith "ожидалось логическое значение"
            )
        |_->failwith "ожидался список"

type ListFilterFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "выбрать"
        member this.Import = Func<|ListPredicateFunc(fun f l->Data<|List.filter f l)
type ListExistsFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "существует"
        member this.Import = Func<|ListPredicateFunc(fun f l->Data<|List.exists f l)
type ListForallFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "дляВсех"
        member this.Import = Func<|ListPredicateFunc(fun f l->Data<|List.forall f l)
type ListFindFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "найти"
        member this.Import = Func<|ListPredicateFunc(List.find)
type ListFindIndexFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "найтиНомер"
        member this.Import = Func<|ListPredicateFunc(fun f l->Data<|List.findIndex f l)
type ListFindBackFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "развернутьНайти"
        member this.Import = Func<|ListPredicateFunc(List.findBack)
type ListFindIndexBackFuncImport() = 
    interface PipelineNamedImportable with
        member this.Name = "развернутьНайтиНомер"
        member this.Import = Func<|ListPredicateFunc(fun f l->Data<|List.findIndexBack f l)
type ListPartitionImport() = 
    interface PipelineNamedImportable with
        member this.Name = "разбить"
        member this.Import = Func<|ListPredicateFunc(fun f l->List.partition f l|>(fun (a,b)->Data<|new Marker("пара",Data(b)::Data(a)::[])))