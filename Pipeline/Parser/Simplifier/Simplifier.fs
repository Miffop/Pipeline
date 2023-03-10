namespace Pipeline.Parser.Simplifier

open Pipeline.Parser.Tokens

type CodeReplacement = {NewCode:Token list;Length:int;Resimplify:bool}

type [<AbstractClass>] ISimplification() = 
    abstract TrySimplify:code:Token list*index:int*length:int*sp:Simplifier->CodeReplacement option

and Simplifier(simplifications:ISimplification seq) = 
    member this.Simplify(code:Token list) = this.Simplify(code,0,code.Length)
    member this.Simplify(code:Token list,index:int,length:int) =
        let mutable code = code
        let mutable i = 0
        let mutable length = length
        while i<length do
            
            let replacementOption =
                simplifications
                |>Seq.choose(fun s->s.TrySimplify(code,index+i,length-i,this))
                |>Seq.tryExactlyOne
            match replacementOption with
            |Some(replacement) ->
                let rep = if replacement.Resimplify then this.Simplify(replacement.NewCode) else replacement.NewCode
                length<-length+rep.Length-replacement.Length
                code<-code.[0..index+i-1]@rep@code.[index+i+replacement.Length..]
                if replacement.Resimplify then
                    i<-i+1
                else
                    i<-i+rep.Length
            |None->
                i<-i+1
        code