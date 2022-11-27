namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type ForSimplification() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) =
        let sc = 
            code.[index..]
            |>List.map(fun t->t.Type,t.Content)
        if not(sc.[0] = ("Keyword","для") && sc.[1] |> fst = "Word" && sc.[2] = ("Keyword","в")) then
            None
        else
            let seqDefLength = 
                let mutable len = 0
                let braceCounter = ref 0
                while !braceCounter>0 || sc.[len+3]<>("Keyword","выполнить") && sc.[len+3]<>("Keyword","вернуть") do
                    match sc.[len+3] with
                    |"BraceOpen",_->incr braceCounter
                    |"BraceClose",_->decr braceCounter
                    |_->()
                    len<-len+1
                len
            let funLength = 
                code.[index+3+seqDefLength+1..]
                |>List.takeWhile(fun x->x>=code.[index+3+seqDefLength+1])
                |>List.length
                |>(fun x->if code.[index+3+seqDefLength+x].Type = "Operation" then x-1 else x)
            let enclose (c:Token list)=(Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::[]) 
            let mapFunc = 
                let first = code.[index+3+seqDefLength+1]
                Token("Keyword","от",first)::Token("Word",snd sc.[1],first)::enclose(code.[index+4+seqDefLength..index+3+seqDefLength+funLength])
                |>enclose
            let seqDef = 
                enclose(code.[index+3..index+2+seqDefLength])
            let mapOrIter =     
                if code.[index+3+seqDefLength].Content = "вернуть" then
                    "отобразить"
                else
                    "перебрать"
            Some<|{CodeReplacement.NewCode = Token("Word",mapOrIter,code.[index])::mapFunc@seqDef;Length = 3+seqDefLength+1+funLength;Resimplify=false}
