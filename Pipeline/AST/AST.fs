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

//AST Nodes

[<AbstractClass>]
type IExpression() = 
    abstract Eval:contex:PContext->PFunOrData


// Seq
type PSeq = System.Collections.IEnumerable

[<AutoOpen>]
module PSeqExtentions =
    type System.Collections.IEnumerable with
        member this.Seq = this |> Seq.cast<obj>

//Lazy
type PLazy(exp:IExpression,c:PContext) = 
    member this.Value = exp.Eval(c)
