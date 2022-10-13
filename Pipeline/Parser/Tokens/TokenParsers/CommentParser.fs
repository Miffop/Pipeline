namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type CommentParser() =
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if code.[index] = '#' then
                let mutable cnt = 0
                while code.[index+cnt]<>'\n' do
                    cnt<-cnt+1
                cnt+1
            else
                0
        member this.GetToken (code,index) =
            None
type WhitespaceParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if System.Char.IsWhiteSpace(code.[index]) then 1 else 0
        member this.GetToken (code,index) =
            None