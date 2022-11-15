namespace Pipeline.Parser.Tokens

type TokenContent(toktype:string,content:string) = 
    member this.Type = toktype
    member this.Content = content
    override this.ToString() = sprintf "(%s # %s)" toktype content

type codeLocation = {CodeRef:string ref;SI:int;EI:int}

type Token(toktype:string,content:string,margin:int,offset:int,line:int,loc:codeLocation) = 
    new (tc:TokenContent,margin,offset,line,loc) = new Token(tc.Type,tc.Content,margin,offset,line,loc)
    new (content:string,t:Token) = new Token(t.Type,content,t.Margin,t.Offset,t.Line,t.Location)
    new (toktype:string,content:string,t:Token) = new Token(toktype,content,t.Margin,t.Offset,t.Line,t.Location)
    member this.Type = toktype
    member this.Content = content
    member this.Line = line
    member this.Margin = margin //отступ всей строки в которой находиться токен
    member this.Offset = offset //положение в строке за вычетом отступа
    member this.TokenContent = new TokenContent(toktype,content)
    member this.Location = loc
    
    override this.ToString() = this.TokenContent.ToString()

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



open System.Collections.Generic

type ITokenParser = 
    abstract GetLength:code:string*index:int->int
    abstract GetToken:code:string*index:int*prev:List<Token>->TokenContent option
type TokenParser(parser:ITokenParser seq) = 
    member this.Parse (code:string) =
        let codeRef = ref code
        let mutable index = 0
        let mutable line = 1
        let mutable margin = 0
        let mutable offset = 0
        let mutable isMargin = true
        let TokList = List<Token>()

        while index<code.Length do
            let tokOption,tokLength=
                parser
                |> Seq.maxBy(fun p->p.GetLength(code,index))
                |> (fun p->p.GetToken(code,index,TokList),p.GetLength(code,index))
            match tokOption with
            |Some(tok)->
                TokList.Add(Token(tok,margin,offset,line,{CodeRef = codeRef;SI = index;EI = index+tokLength-1}))
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
                    failwith "табы запрешенны"
                |_ ->
                    offset<-offset+1
                    isMargin<-false
            index<-index+tokLength
        TokList |> List.ofSeq