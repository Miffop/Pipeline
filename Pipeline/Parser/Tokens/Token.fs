namespace Pipeline.Parser.Tokens

type TokenContent(toktype:string,content:string) = 
    member this.Type = toktype
    member this.Content = content

type Token(toktype:string,content:string,margin:int,offset:int,line:int) = 
    new (tc:TokenContent,margin,offset,line) = new Token(tc.Type,tc.Content,margin,offset,line)
    member this.Type = toktype
    member this.Content = content
    member this.Line = line
    member this.Margin = margin //отступ всей строки в которой находиться токен
    member this.Offset = offset //положение в строке за вычетом отступа
    member this.TokenContent = new TokenContent(toktype,content)
    
    interface System.IComparable with
        member this.CompareTo other = 
            match other with
            | :?Token as other ->
                let marginDifference = this.Margin-other.Margin
                let offsetDifference = this.Offset-other.Offset
                if marginDifference<>0 then
                    marginDifference
                else
                    offsetDifference
            |_ -> -1
    override this.Equals obj =
        match obj with
        | :?Token as other->(this:>System.IComparable).CompareTo(other)=0
        |_->false
    override this.GetHashCode() =
        this.Margin+this.Offset


type ITokenParser = 
    abstract GetLength:code:string*index:int->int
    abstract GetToken:code:string*index:int->TokenContent option
type TokenParser(parser:ITokenParser seq) = 
    member this.Parse (code:string) =
        let mutable index = 0
        let mutable line = 1
        let mutable margin = 0
        let mutable offset = 0
        let mutable isMargin = true
        let TokList = System.Collections.Generic.List<Token>()

        while index<code.Length do
            let tokOption,tokLength=
                parser
                |> Seq.maxBy(fun p->p.GetLength(code,index))
                |> (fun p->p.GetToken(code,index),p.GetLength(code,index))
            match tokOption with
            |Some(tok)->TokList.Add(Token(tok,margin,offset,line))
            |None->()
            for c in code.Substring(index,tokLength) do                
                match c with
                |'\n'->
                    line<-line+1
                    margin<-0
                    offset<-0
                    isMargin<-true
                |' ' when isMargin->
                    margin<-margin+1
                |'\t'->
                    raise<|System.Exception("tabs are not allowed")
                |_ ->
                    offset<-offset+1
                    isMargin<-false
            index<-index+tokLength
        TokList