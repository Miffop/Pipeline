namespace Pipeline.AST





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
    
    let defs = System.Collections.Generic.Dictionary<string,PFunOrData>()

    new ()  = new PContext(None)
    
    member this.Parent = parent
    member this.Defenitions = defs

    member this.Find (defname:string) = 
         match defs.TryGetValue(defname),parent with
         |(true,result),_->result
         |(false,_),Some(parent)->parent.Find(defname)
         |_->raise <| new System.Exception()
    member this.Def defname defenition = 
        defs.[defname]<-defenition
    member this.Merge(other:PContext) =
        for def in other.Defenitions do
            this.Def def.Key def.Value
//AST Nodes


type StringImage = 
    {CodeRef:string ref;SI:int;EI:int}
    override this.ToString() = (!this.CodeRef).[this.SI..this.EI]
[<AbstractClass>]
type IExpression(s:StringImage option) = 
    abstract Eval:contex:PContext->PFunOrData
    override this.ToString() = 
        match s with
        |Some(s)->s.ToString()
        |None->sprintf"%s:Отсутствует строковое представление"<|this.GetType().ToString()
        



//Lazy
type PLazy(exp:IExpression,c:PContext) = 
    member this.Value = exp.Eval(c)
