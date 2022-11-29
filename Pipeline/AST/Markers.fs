namespace Pipeline.AST

type Marker(name:string,elem:PFunOrData list) = 
    member this.Name = name
    member this.Elements = elem
    override this.ToString() =
        if elem.IsEmpty then
            name
        else
            let args =
                elem
                |>List.map (function |Data(x)->sprintf "(%O)" x |Func(x)->"(функ.)")
                |>List.reduceBack(fun x a->a+" "+x)
            sprintf "%s %s" name args

type MarkerFunc(name:string,length:int) = 
    inherit PFunc()
    member this.Name = name
    member this.Length = length
    override this.Eval(arg) = 
        match arg with
        |Data(:?Marker as m) when m.Name = name->
            match length with
            |0->failwithf "невозможно извлечь данные маркера %s так он пустой" name
            |1->m.Elements.Head
            |_->failwithf "в макере %s несколько записей" name
        |x when length = 1->
            Data<|Marker(name,arg::[])
        |x ->
            Func<|MarkerFuncCurried(name,length,x::[])
and MarkerFuncCurried(name:string,length:int,elem:PFunOrData list) = 
    inherit PFunc()
     override this.Eval(arg) = 
        match arg,elem with
        |Data(:?Marker as m),(Data(:?int as x)::[]) when m.Name = name->
            m.Elements.[m.Elements.Length-1-x]
        |_->
            if length=elem.Length+1 then
                Data<|Marker(name,arg::elem)
            else
                Func<|MarkerFuncCurried(name,length,arg::elem)

type MarkerCheckFunc() = 
    inherit PFunc()
    override this.Eval(arg) = 
        match arg with
        |Data(:?Marker as arg)->
            Func<|MarkerCheckCurriedFunc(arg.Name)
        |Func(:?MarkerFunc as arg)->
            Func<|MarkerCheckCurriedFunc(arg.Name)
        |_->failwith ""
    override this.ToString() = sprintf "(?) ... ..."
and MarkerCheckCurriedFunc(name:string) = 
    inherit PFunc()
    override this.Eval(arg) =
        match arg with
        |Data(:?Marker as m)->
            Data(m.Name = name)
        |_->
            raise<|WrongTypeException(this.ToString(),typeof<Marker>,arg.GetType())
    override this.ToString() = sprintf "(?) %s ..." name