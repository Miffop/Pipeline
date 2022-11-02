namespace Pipeline.Parser.Expressions

open Pipeline.AST
open Pipeline.Parser.Tokens

type IOperationExpressionParser = 
    abstract GetPriority:op:Token->int
    abstract GetExpression:op:Token*left:IExpression*right:IExpression->IExpression
and IExpressionParser = 
    abstract GetExpression:code:Token seq*index:int*length:int*ep:CodeParser->IExpression option

and CodeParser(expParser:IExpressionParser seq,opParser:IOperationExpressionParser seq) =
    
    
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
            
            let left = this.ParseExpression(code,index,operOffset)
            let right = this.ParseExpression(code,index+1,length-operOffset-1)
            operParser.GetExpression(oper,left,right)
        else
            expParser
            |> Seq.choose(fun p->p.GetExpression(code,index,length,this))
            |> Seq.exactlyOne

    member this.ParseCodeBlock (code:Token seq,index:int)=
        let first = code |> Seq.item index  
        let codeBlockLength = code |> Seq.indexed |> Seq.filter(fun (i,x)->i>=index) |> Seq.map(fun (i,x)->x) |> Seq.takeWhile(fun x->x>first) |> Seq.length
        this.ParseExpression(code,index,codeBlockLength)