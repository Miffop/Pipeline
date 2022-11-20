namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier


type MonadSimplification() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        if not(code.[index].Type = "Keyword" && (code.[index].Content = "монада" || code.[index].Content = "применить") && code.[index+1].Type = "Word") then
            None
        else
            let len = 
                code.[index+2..]
                |>List.takeWhile(fun x->x>=code.[index+2])
                |>List.length
                |>(fun x->if code.[index+1+x].Type="Operation" then x-1 else x)
            let enclose (c:Token list)=(Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::[]) 
            let def = enclose<|code.[index+2..index+2+len-1]
            Some({NewCode = code.[index..index+1]@def;Length = len+2;Resimplify = false})