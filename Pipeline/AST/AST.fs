namespace Pipeline.AST





//Data and Functions
[<AbstractClass>]
type PFunc() = 
    abstract Eval:argument:PFunOrData->PFunOrData

and PData = obj
and PFunOrData = 
    |Func of PFunc
    |Data of PData
    override this.ToString() = 
        match this with
        |Data(:?(PFunOrData list) as l)->
            l
            |>List.map(fun x->x.ToString())
            |>List.reduce(fun a x->sprintf "%s, %s" a x)
            |>(fun x->sprintf "(%s,[])" x)
        |Data(:?bool as b)->if b then "правда" else "ложь"

        |Data(d)->d.ToString();
        |Func(f)->f.ToString();

//Contex
and PContext(parent:PContext option,monad:PMonad) = 
    
    let defs = System.Collections.Generic.Dictionary<string,(PFunOrData*int) list>()

    new (monad)  = new PContext(None,monad)
    
    member this.Parent = parent
    member this.Defenitions = defs
    member this.Monad = monad

    member this.Find (defname:string)(location:int) = 
         if defname = "_" then 
            failwithf "_ не пременная"
         else
         match defs.TryGetValue(defname),parent with
         |(true,result),_ when (result|>List.last|>snd)<=location
            ->result |> List.find(fun x->(snd x)<=location) |> fst
         |(false,_),Some(parent)->parent.Find(defname)(location)
         |_->failwithf "определение для %s не найдено" defname
    member this.Def defname location defenition = 
        if defname = "_" then 
            ()
        else
        if defs.ContainsKey(defname) then
            defs.[defname]<-(defenition,location)::defs.[defname]
        else
            defs.[defname]<-(defenition,location)::[]
    member this.Merge(other:PContext) =
        for def in other.Defenitions do
            this.Def def.Key -1 (def.Value.Head |> fst)
and [<AbstractClass>] PMonad() =
    abstract Return:PFunc
    abstract Bind:PFunOrData*PFunc->PFunOrData
    abstract Then:PFunOrData*PFunOrData->PFunOrData
    default this.Then (k,m) = 
        this.Bind(k,{
            new PFunc() with
            override this.Eval(arg) = 
                m
        })
//AST Nodes


type StringImage = 
    {CodeRef:string ref;SI:int;EI:int}
    override this.ToString() = (!this.CodeRef).[this.SI..this.EI]
[<AbstractClass>]
type IExpression(s:StringImage option) = 
    abstract Eval:contex:PContext->PFunOrData
    member this.Location =
        match s with
        |Some(s)->(s.SI)
        |None->(-1)
    override this.ToString() = 
        match s with
        |Some(s)->s.ToString()
        |None->sprintf"%s:Отсутствует строковое представление"<|this.GetType().ToString()
        



//Lazy
type PLazy(exp:IExpression,c:PContext) = 
    member this.Value = exp.Eval(c)
