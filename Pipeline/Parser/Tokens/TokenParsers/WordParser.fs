namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type WordParser(keyWords:string list) = 
    interface ITokenParser with
        member this.GetLength (code,index) = 
            if System.Char.IsLetter(code.[index]) then
                let mutable cnt = 0
                while System.Char.IsLetterOrDigit(code.[index+cnt]) do
                    cnt<-cnt+1
                cnt
            else
                0
        member this.GetToken (code,index,prev) = 
            let mutable str = ""
            let mutable cnt = 0
            while System.Char.IsLetterOrDigit(code.[index+cnt]) do
                str<-str+string(code.[index+cnt])
                cnt<-cnt+1
            match str with
            |s when List.exists(fun x->x=s) keyWords->Some<|TokenContent("Keyword",s)
            |s->Some<|TokenContent("Word",s)

type QuoteWordParser() = 
    interface ITokenParser with
        member this.GetLength(code,index)=
            if code.[index]='`' then
                let mutable cnt = 1
                while code.[index+cnt]<>'`' do
                    cnt<-cnt+1
                cnt+1
            else
                0
        member this.GetToken(code,index,prev) = 
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index+1,x-2))
            |>(fun x->TokenContent("Word",x))
            |>Some
type WildCardParser() = 
    interface ITokenParser with
        member this.GetLength(code,index)=
            if code.[index]='_' then
                1
            else
                -1
        member this.GetToken(code,index,prev) = 
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index,x))
            |>(fun x->TokenContent("Word",x))
            |>Some