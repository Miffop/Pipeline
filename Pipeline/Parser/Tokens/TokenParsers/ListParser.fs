namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type ListParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) =
            let str = 
                code.[index..]
                |>List.ofSeq
            match str with
            |'['::']'::_
                -> 2
            |','::_
                -> 1
            |_->(-1)
        member this.GetToken(code,index,prev) =
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index,x))
            |>(fun x->if (x="[]") then TokenContent("Word","пустойСписок") else TokenContent("Operation",x))
            |>Some