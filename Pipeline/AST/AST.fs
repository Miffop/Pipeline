namespace Pipeline.AST


type PData = obj
type PFunc = delegate of obj->obj

type PFunOrData = 
    |Fun of PFunc
    |Data of PData

type PContex(parent:PContex option) = 
    
    let mutable object:obj = null
    let defs = System.Collections.Generic.Dictionary<string,PFunOrData>()

    new ()  = new PContex(None)
    
    member this.Parent = parent
    member this.Object with get() = object and set v = object<-v
    member this.Defenitions = defs

    member this.Find (defname:string) = 
         match defs.TryGetValue(defname),parent with
         |(true,result),_->result
         |(false,_),Some(parent)->parent.Find(defname)
         |_->raise <| new System.Exception()
    member this.Add = defs.Add

type ICommand =
    abstract Exec:PContex->PData

type IExpression = 
    abstract Eval:PContex->PFunOrData