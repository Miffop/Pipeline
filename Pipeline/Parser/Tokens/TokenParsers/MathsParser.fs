namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type ArithmeticParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            match code.[index] with
            |'+'|'-'|'*'|'/'|'%'->1
            |_->0
        member this.GetToken(code,index,prev) =
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index,x))
            |>(fun x->TokenContent("Operation",x))
            |>Some