namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type IfSimplification() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        if not(code.[index].Type = "Keyword" && code.[index].Content = "если") then
            None
        else
            
            let findLen offset (word:string seq)=
                let mutable braceCounter = 0
                let mutable nestedIfCounter = 0
                let mutable nestedForCounter = 0
                let mutable len = 0
                while not(code.[index+len+offset].Type = "Keyword" && Seq.exists((=)code.[index+len+offset].Content) word && braceCounter = 0 && nestedIfCounter = 0) do
                    match code.[index+len+offset].Type,code.[index+len+offset].Content with
                    |"Keyword","если"->
                        nestedIfCounter<-nestedIfCounter+1
                    |"Keyword","иначе"->
                        nestedIfCounter<-nestedIfCounter-1
                    |"Keyword","для"->
                        nestedForCounter<-nestedForCounter+1
                    |"BraceOpen",_->
                        braceCounter<-braceCounter+1
                    |"BraceClose",_->
                        braceCounter<-braceCounter-1
                    |_->()
                    len<-len+1
                len
            
            let offset1 = 1
            let condLen = findLen offset1 ["то";"вернуть"]
            let offset2 = 1 + condLen + 1
            let thenLen = findLen offset2 ["иначе"]
            let offset3 = 1 + condLen + 1 + thenLen + 1
            let elseLen = 
                let len = 
                    let elemElse = code.[index+offset3-1]
                    let elem = code.[index+offset3] 
                    code.[index+offset3..]
                    |>List.takeWhile(fun x->x>=elem || (x=elemElse && x.Content = "иначе" && x.Type="Keyword"))
                    |>List.length
                let last = code.[index+offset3+len-1]
                if last.Type = "Operation" then
                    len-1
                else
                    len

            let totalLen = 1 + condLen + 1 + thenLen + 1 + elseLen
            let encloseLazy c=(Token("BraceOpen","(",List.head c)::Token("Keyword","lazyBlock",List.head c)::Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::Token("BraceClose","(",List.last c)::[]) 
            
            let enclose (c:Token list)=(Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::[]) 

            let condCode = code.[index+offset1..index+offset1+condLen-1] |> enclose
            let thenCode = code.[index+offset2..index+offset2+thenLen-1] |> encloseLazy
            let elseCode = code.[index+offset3..index+offset3+elseLen-1] |> encloseLazy

            Some({NewCode = (Token("Word","если",code.[index])::condCode)@thenCode@elseCode;Length = totalLen;Resimplify = false})
