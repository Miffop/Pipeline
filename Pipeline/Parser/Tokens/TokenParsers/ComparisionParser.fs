namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens


type ComparisionParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            let snd = if index+1<code.Length then code.[index+1] else ' '
            match code.[index],snd with
            |'>','='
            |'<','='
            |'!','='
                ->2
            |'?',_
            |'=',_
            |'>',_
            |'<',_
                ->1
            |_->0
        member this.GetToken(code,index,prev) =
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index,x))
            |>(fun x->TokenContent("Operation",x))
            |>Some