namespace Pipeline.Parser.Expressions

open Pipeline.AST
open Pipeline.Parser.Tokens

type EmptyExpression() = 
    inherit IExpression(None)
    override this.Eval(c) = Func(Identity())

type [<AbstractClass>] IOperationExpressionParser() = 
    abstract GetPriority:op:Token->int
    abstract GetOrderDirection:op:Token->bool //true - leftToRight, false -  rightToLeft
    default this.GetOrderDirection(op) = true
    abstract GetExpression:op:Token*left:IExpression*right:IExpression*strImage:StringImage->IExpression // if there is no right or left expression there will be empty expression

and [<AbstractClass>] IExpressionParser() = 
    abstract GetExpression:code:Token list*index:int*length:int*ep:ExpressionParser->IExpression option

and ExpressionParser(expParser:IExpressionParser seq,opParser:IOperationExpressionParser seq) =
    
    
    member this.ParseExpression(code:Token list,index:int,length:int) = 
        if length = 0 then 
            EmptyExpression() :> IExpression
        else
        let ops = [
            let mutable i = 0
            let mutable braceCounter = 0
            while i<length do
                match (Seq.item (index+i) code).Type with
                |"Operation" when braceCounter=0 -> yield (i,Seq.item (index+i) code)
                |"BraceOpen" -> braceCounter<-braceCounter+1
                |"BraceClose" -> braceCounter<-braceCounter-1
                |_ -> ()
                i<-i+1
        ]
        if ops.Length<>0 then
            
            let operOffset,oper,(operParser,operPriority) = 
                ops
                |>Seq.map(fun (i,x)->i,x,opParser|>Seq.map(fun p->p,p.GetPriority x)|>Seq.filter(fun (_,p)->p<> -1)|>Seq.exactlyOne)
                |>Seq.groupBy(fun (i,x,(p,pr))->(pr,p))
                |>Seq.maxBy(fun ((pr,p),inf)->pr)
                |>snd
                |>Seq.maxBy(fun (i,x,(p,pr))->if p.GetOrderDirection(x) then i else -i)
            
            let left = this.ParseExpression(code,index,operOffset)
            let right = this.ParseExpression(code,index+operOffset+1,length-operOffset-1)
            operParser.GetExpression(oper,left,right,this.CreateStringImage(code,index,length))
            
        else
            let expOption = 
                expParser
                |> Seq.choose(fun p->p.GetExpression(code,index,length,this))
                |> Seq.tryExactlyOne
            match expOption with
            |Some(exp)->exp
            |None->
                //Application
                let last = Seq.item (index+length-1) code
                let mutable len = 1
                if last.Type = "BraceClose" then
                    let braceCounter = ref 1
                    while !braceCounter>0 do
                        match (Seq.item (index+length-len-1) code).Type with
                        |"BraceClose"->incr braceCounter
                        |"BraceOpen"->decr braceCounter
                        |_->()
                        len<-len+1
                let strImage = 
                    this.CreateStringImage(code,index,length)
                Pipeline.AST.Expressions.ApplyExpression(this.ParseExpression(code,index,length-len),this.ParseExpression(code,index+length-len,len),Some<|strImage)      
    member this.CreateStringImage (ST:Token,ET:Token)=   
        if ST.Location.CodeRef <> ET.Location.CodeRef then raise<|System.NotImplementedException()
        {CodeRef = ST.Location.CodeRef;StringImage.SI = ST.Location.SI;StringImage.EI = ET.Location.EI}
    member this.CreateStringImage (code:Token list,index:int,length:int) = 
        let ST = code.[index]
        let ET = code.[index+length-1]
        this.CreateStringImage(ST,ET)

    member this.ParseCodeBlock (code:Token list,index:int)=
        let first = code |> Seq.item index  
        let codeBlockLength = code |> Seq.indexed |> Seq.filter(fun (i,x)->i>=index) |> Seq.map(fun (i,x)->x) |> Seq.takeWhile(fun x->x>=first) |> Seq.length
        this.ParseExpression(code,index,codeBlockLength)
