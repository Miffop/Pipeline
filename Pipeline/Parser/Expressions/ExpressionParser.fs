namespace Pipeline.Parser.Expressions

open Pipeline.AST
open Pipeline.Parser.Tokens

type IOperationExpressionParser = 
    abstract GetPriority:op:Token->int
    abstract GetExpression:op:Token*left:IExpression*right:IExpression->IExpression
and IExpressionParser = 
    abstract GetExpression:code:Token seq*index:int*length:int*ep:CodeParser->IExpression option
and IStructParser = 
    abstract Parse:code:Token seq*index:int*ep:CodeParser->(IExpression*int)option

and CodeParser(structParser:IStructParser seq,expParser:IExpressionParser seq,opParser:IOperationExpressionParser seq) =
    
    member this.ParseCommand (code:Token seq,index:int) = 
        let structOption = 
            structParser
            |>Seq.choose(fun x->x.Parse(code,index,this))
            |>Seq.tryExactlyOne
        match structOption with
        |Some(structure)->structure
        |None->
            let length = 
                code
                |>Seq.findIndex(fun x->x.Type="EndLine")
            (this.ParseExpression(code,index,length),(length+1))
    
    member this.ParseExpression(code:Token seq,index:int,length:int) = 
        let ops = [
            let mutable i = 0
            let mutable braceCounter = 0
            while i<length do
                match (Seq.item (index+i) code).Type with
                |"Operation" when braceCounter=0 -> yield (i,Seq.item (index+i) code)
                |"BraceOpen" -> braceCounter<-braceCounter+1
                |"BraceClose" -> braceCounter<-braceCounter-1
                |_ -> ()
        ]
        if ops.Length<>0 then
            let operOffset,oper,(operParser,operPriority) = 
                ops
                |>Seq.map(fun (i,x)->i,x,opParser|>Seq.map(fun p->p,p.GetPriority x)|>Seq.filter(fun (_,p)->p<> -1)|>Seq.exactlyOne)
                |>Seq.maxBy(fun (i,x,(p,pr))->pr)
            
            let left = this.ParseExpression(code,index,i)
            let right = this.ParseExpression(code,index+1,length-operOffset-1)
            operParser.GetExpression(oper,left,right)
        else
            expParser
            |> Seq.choose(fun p->p.GetExpression(code,index,length,this))
            |> Seq.exactlyOne

    member this.ParseCodeBlock (code:Token seq,index:int)=
        let commandList = 
            Seq.unfold(fun i->
                if i<Seq.length code && (Seq.item (index+i) code)>(Seq.item index code) then 
                    let commandExpression,commandLength = this.ParseCommand(code,index+i)
                    let i = commandLength+i
                    Some((commandExpression,i),i)
                else 
                    None
            ) index
        CodeBlock(commandList|>Seq.map(fun (x,_)->x)),(commandList|>Seq.last|>snd)
    