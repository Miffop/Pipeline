namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type WordParser() = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if System.Char.IsLetter(code.[index]) then
                let mutable cnt = 0
                while System.Char.IsLetterOrDigit(code.[index+cnt]) do
                    cnt<-cnt+1
                cnt
            else
                0
        member this.GetToken (code,index) = 
            let mutable str = ""
            let mutable cnt = 0
            while System.Char.IsLetterOrDigit(code.[index+cnt]) do
                str<-str+string(code.[index+cnt])
                cnt<-cnt+1
            match str with
            |s->Some<|TokenContent("Word",s)
