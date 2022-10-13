namespace Pipeline.AST

//Data
type PData = obj
type PFunc = delegate of obj->obj
type PFunOrData = 
    |Fun of PFunc
    |Data of PData

type PSeq = System.Collections.IEnumerable

[<AutoOpen>]
module PSeqExtentions =
    type System.Collections.IEnumerable with
        member this.Seq = this |> Seq.cast<obj>


//Contex
type PContex(parent:PContex option) = 
    
    let defs = System.Collections.Generic.Dictionary<string,PFunOrData>()

    new ()  = new PContex(None)
    
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

type IExpression = 
    abstract Eval:contex:PContex*Object:obj->PFunOrData