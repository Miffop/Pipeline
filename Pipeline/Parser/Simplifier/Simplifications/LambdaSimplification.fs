namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type LambdaSimplification() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        if not(code.[index].Type="Keyword" && code.[index].Content = "от" && code.[index+1].Type="Word")then 
            None
        else
            let mutable len = 0;
            let mutable braceCounter = 0;
            while len+2<length && (braceCounter>0 || (braceCounter=0 && code.[index+2]<=code.[index+2+len])) do
                match code.[index+2+len].Type with
                |"BraceOpen"->
                    braceCounter<-braceCounter+1
                |"BraceClose"->
                    braceCounter<-braceCounter-1
                |_->()
                len<-len+1
            if braceCounter=(-1) then
                len<-len-1
            if len+2<length && code.[index+2+len].Type = "Operation" then
                len<-len-1
            let enclose (c:Token list)=(Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::[]) 
            let def = enclose<|code.[index+2..index+2+len-1]

            Some({NewCode = code.[index..index+1]@def;Length = len+2;Resimplify = false})

