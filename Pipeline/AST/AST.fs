﻿namespace Pipeline.AST





//Data and Functions
[<AbstractClass>]
type PFunc() = 
    abstract Eval:argument:PFunOrData->PFunOrData

and PData = obj
and PFunOrData = 
    |Func of PFunc
    |Data of PData

//Contex
and PContext(parent:PContext option) = 
    
    let defs = System.Collections.Generic.Dictionary<string,(PFunOrData*int) list>()

    new ()  = new PContext(None)
    
    member this.Parent = parent
    member this.Defenitions = defs

    member this.Find (defname:string)(location:int) = 
         match defs.TryGetValue(defname),parent with
         |(true,result),_ when (result|>List.last|>snd)<=location
            ->result |> List.find(fun x->(snd x)<=location) |> fst
         |(false,_),Some(parent)->parent.Find(defname)(location)
         |_->raise <| new System.Exception()
    member this.Def defname location defenition = 
        if defs.ContainsKey(defname) then
            defs.[defname]<-(defenition,location)::defs.[defname]
        else
            defs.[defname]<-(defenition,location)::[]
    member this.Merge(other:PContext) =
        for def in other.Defenitions do
            this.Def def.Key -1 (def.Value.Head |> fst)
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
