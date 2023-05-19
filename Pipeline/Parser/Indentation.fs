namespace Pipeline.Parser

[<CustomComparison>]
[<CustomEquality>]
type Position = 
    {
        Margin:int
        Offset:int
    }
    interface System.IComparable with
        member this.CompareTo x =
            match x with
            | :?Position as x ->
                match this.Margin - x.Margin with
                |0 -> this.Offset - x.Offset
                |x -> x
            |_->failwith "ожидалась позиция"
    override this.Equals o =
        match o with
        | :? Position as p -> this.Margin = p.Margin && this.Offset = p.Offset 
        | _ -> false
    override this.GetHashCode() = 
        this.Margin + this.Offset
module Indentation = 
    open Parser.Ops

    
    let line indentToken resetToken = 
        Parser.Monad(){
            let! indetationSpaces = Parser.any(Parser.token indentToken)
            let! restOfTheLine = Parser.any(Parser.satisfy((<>)resetToken))
            let! _ = Parser.token resetToken
            let indentaion = List.length indetationSpaces
            return List.mapi(fun i t->t,{Margin = indentaion;Offset = i})restOfTheLine
        }
    let text indentToken resetToken =
        List.concat<^>Parser.any(line indentToken resetToken)
    

    
    let private position : Parser<Position,'t*Position> = P(
        function
        |(t,p)::ts->Some(p,(t,p)::ts)
        |[]->Some({Margin = -1;Offset = 0},[]) //вообще не классная строчка
    )
    let ret x = 
        Parser.Monad(){
            let! pos = position
            return (x,pos)
        }
    let satisfy c : Parser<'a*Position,'a*Position> = 
        Parser.satisfy(fst>>c)
    let token t = 
        satisfy ((=)t)
    type Monad() = 
        member this.Return x = ret x
        member this.ReturnFrom x = x
        member this.Bind(Mx,f) =
            Parser.Monad(){
                let! x,pos = Mx
                let! y,_ = f x
                return (y,pos)
            }
    [<AutoOpen>]
    module Ops = 
        let (<*>) p1 p2 = 
            Monad(){
                let! f = p1
                let! x = p2
                return f x
            }
        let (<.>) p1 (p2:Parser<'a*Position,'t*Position> Lazy) =
            Monad(){
                let! f = p1
                let! x = p2.Value
                return f x
            }
        let (<^>) f p = ret f <*> p
        let (<|>) = (<|>)
        
        let (<^) f p = (fun _->f) <^> p
        let (<*) p q = (fun x _ ->x) <^> p <*> q
        let ( *>) p q = (fun _ y->y) <^> p <*> q
        let (<**>) p q = (fun x y -> y x) <^> p <*> q
        let (<??>) p q = p<**>(q<|>lazy(ret id))
        

    let any p = 
        Parser.Monad(){
            let! pos = position
            let! x = Parser.any p
            return List.map fst x,pos
        }
    let some p = 
        Parser.Monad(){
            let! pos = position
            let! x = Parser.some p
            return List.map fst x,pos
        }
    let option p = 
        (Some<^>p)<|>lazy(ret None)
   
    let flip f x y = f y x
    let mapTokens (mapping:Map<'t,'x>) = (fun x->mapping[x]) <^> satisfy(flip Map.containsKey mapping)
    let pack o p c = (token o)*>p<*(token c)
    let packLazy o p c = (fun _ x _ -> x)<^>token o<.>p<*>token c
    let prefix pref p = (token pref)*>p
    let prefixLazy pref p = (fun _ x -> x)<^>(token pref)<.> p
    let rec pattern s = 
        match s with
        |s::[] -> (fun x->x::[])<^>token s
        |s::ss -> (fun x y->x::y)<^>token s<*>pattern ss
        |[]    -> Parser.fail
    
    let rec private applyAll x fs=
        match fs with
        |f::fs -> applyAll(f x)fs
        |[]->x
    let chain op p = 
        applyAll<^>p<*>any(flip<^>op<*>p)
    let rec chainBack op p = 
        p<??>(flip<^>op<.>lazy(chainBack op p))
    let bindOption f p = 
        Monad(){
            let! x = p
            match f x with
            |Some(x)->return x
            |None->return! Parser.fail
        }
    let getPosition = P(
        function
        |(t,p)::ts->Some((p,p),(t,p)::ts)
        |[]->None
    )
    let sameOrIndentedScope openToken closeToken p = P(
        function
        |(t,refp)::ts -> 
            let rec filter d r ts0=
                match ts0 with
                |(t,p)::ts when t = openToken && (d<>0 || p>=refp) -> filter (d+1) ((t,p)::r) ts
                |(t,p)::ts when t = closeToken && d = 0-> r,ts0
                |(t,p)::ts when t = closeToken -> filter (d-1) ((t,p)::r) ts
                |(t,p)::ts when d <> 0 || p>=refp -> filter d ((t,p)::r) ts
                
                |_->r,ts0
            let ts,rest = 
                (t,refp)::ts
                |>filter 0 []
               
            ts
            |>List.rev
            |>Parser.run p
            |>Option.map(fun (x,ts)->x,ts@rest)
        |[] -> Parser.run p []
    )