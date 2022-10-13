namespace Pipeline.Parser.Tokens.TokenParsers

open Pipeline.Parser.Tokens

type IntParser() = 
    interface ITokenParser with
        member this.GetLength (code,index)= 
            let mutable cnt = 0
            while System.Char.IsDigit(code.[index+cnt]) do
                cnt<-cnt+1
            cnt
        member this.GetToken (code,index)=
            (this:>ITokenParser).GetLength(code,index)
            |> (fun x->code.Substring(index,x))
            |> (fun x->TokenContent("Int",x))
            |> Some
type StringParser() = 
    interface ITokenParser with
        member this.GetLength(code,index)=
            if code.[index]='"' then
                let mutable cnt = 1
                while code.[index+cnt]<>'"' do
                    cnt<-cnt+1
                cnt+1
            else
                0
        member this.GetToken(code,index) = 
            (this:>ITokenParser).GetLength(code,index)
            |>(fun x->code.Substring(index+1,x-2))
            |>(fun x->TokenContent("String",x))
            |>Some
type FloatParser() = 
    interface ITokenParser with
        member this.GetLength(code,index) = 
            let mutable cnt = 0
            while System.Char.IsDigit(code.[index+cnt]) || code.[index+cnt]='.' do
                cnt<-cnt+1
            cnt
        member this.GetToken (code,index)=
            (this:>ITokenParser).GetLength(code,index)
            |> (fun x->code.Substring(index,x))
            |> (fun x->TokenContent("Float",x))
            |> Some