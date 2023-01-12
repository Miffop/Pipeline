namespace Pipeline.AST

// Seq


//type PSeq = seq<PFunOrData>
type PList = PFunOrData list



(*[<AutoOpen>]
module PSeqExtentions =
    type System.Collections.IEnumerable with
        member this.Seq = this |> Seq.cast<obj>
*)