namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type DefineSimplification() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        if not(code.[index].Type="Keyword" && code.[index].Content = "пусть") then 
            None
        else
            if code.[index+1].Type <> "Word" then
                raise<|System.NotImplementedException()
            else
            let defName = code.[index+1].Content
            let defLen = 
                let elem = code.[index+2]
                code.[index+2..]
                |>List.takeWhile(fun x->x>=elem)
                |>List.length
                |>(fun x->if code[index+1+x].Type = "Operation" then x-1 else x)
            let enclose (c:Token list)=(Token("BraceOpen","(",List.head c)::sp.Simplify(c))@(Token("BraceClose","(",List.last c)::[]) 
            let def = enclose<|code.[index+2..index+2+defLen-1]
            Some({NewCode = code.[index..index+1]@def;Length = defLen+2;Resimplify = false})

