namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type CommentParser() =
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if code.[index] = '#' then
                let mutable cnt = 0
                while code.[index+cnt]<>'\n' do
                    cnt<-cnt+1
                cnt
            else
                0
        member this.GetToken (code,index,pipe) =
            None
type WhitespaceParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if System.Char.IsWhiteSpace(code.[index]) then 1 else 0
        member this.GetToken (code,index,prev) =
            if prev.Count = 0 then
                None
            else
            match code.[index]='\n',prev.[prev.Count-1] with
            |_,x when x.Type="Operation" || x.Type="Keyword" -> None
            |true,_ -> Some<|TokenContent("Operation","|!>")
            |_->None