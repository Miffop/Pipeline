namespace Pipeline.AST

// Seq


type PSeq = seq<PFunOrData>

(*[<AutoOpen>]
module PSeqExtentions =
    type System.Collections.IEnumerable with
        member this.Seq = this |> Seq.cast<obj>
*)