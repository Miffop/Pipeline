namespace Pipeline.Parser


type Parser<'s,'t> = 
    |P of ('t list -> ('s*'t list)option)

open Pipeline

module Parser = 
    let run (P p) t = 
        p t
    let ret x = P(fun t->Some(x,t))
    let fail = P(fun _->None)
    let bind f px = P(fun t-> 
        OptionMonad(){
            let! x,t1 = run px t
            return! run (f x) t1
        }
    )
    type Monad() = 
        member this.Return x = ret x
        member this.ReturnFrom x = x
        member this.Bind (px,f) = bind f px
    [<AutoOpen>]
    module Ops = 
        let (<|>) p1 (p2:Parser<'a,'t> Lazy) = P(fun t->
            match run p1 t with
            |None->run p2.Value t
            |x->x
        )
        let (<*>) pf px = 
            Monad(){
                let! f = pf
                let! x = px
                return (f x)
            }
        let (<.>) pf (px:Parser<'a,'t> Lazy) = 
            Monad(){
                let! f = pf
                let! x = px.Value
                return (f x)
            }
        let (<^>) f px = 
            ret f <*> px
        let (<^) f p = (fun _->f) <^> p
        let (<*) p q = (fun x _ ->x) <^> p <*> q
        let ( *>) p q = (fun _ y->y) <^> p <*> q
        let (<**>) p q = (fun x y -> y x) <^> p <*> q
        let (<??>) p q = p<**>(q<|>lazy(ret id))
    let choose ps = 
        List.fold(<|>)fail ps

    let satisfy c = P(
        function
        |t::ts when c t -> Some(t,ts)
        |_->None
    )
    let token t = satisfy((=)t)
    let ending = P(
        function
        |[]->Some((),[])
        |_->None
    )
    let terminated p =
        Monad(){
            let! x = p
            do! ending
            return x
        }
    
    let any p = P(fun t->
        let mutable ts = t
        let s = List.unfold(fun t->ts<-t;run p t)t
        Some(s,ts)
    )
    let some p = 
        Monad(){
            let! x = p
            let! xs = any p
            return x::xs
        }
    let option p = 
        (Some<^>p)<|>lazy(ret None)
    let rec private applyAll x fs=
        match fs with
        |f::fs -> applyAll(f x)fs
        |[]->x
    let private flip f x y = 
        f y x
    let chain op p = 
        applyAll<^>p<*>any(flip<^>op<*>p)
    let rec chainBack op p = 
        p<??>(flip<^>op<.>lazy(chainBack op p))
    
    let mapToken t x =  (fun _->x)<^>token t
    let mapTokens (mapping:Map<'t,'x>) = (fun x->mapping[x]) <^> satisfy(flip Map.containsKey mapping)

    let pack o p c = (token o)*>p<*(token c)
    let packLazy o p c = (fun _ x _ -> x)<^>token o<.>p<*>token c
    let prefix pref p = (token pref)*>p
    let prefixLazy pref p = (fun _ x -> x)<^>(token pref)<.> p


    let rec pattern s = 
        match s with
        |s::[] -> (fun x->x::[])<^>token s
        |s::ss -> (fun x y->x::y)<^>token s<*>pattern ss
        |[]    -> fail
    let one = P(
        function
        |t::ts->Some(t,ts)
        |[]->None
    )
    let bindOption f p = 
        Monad(){
            let! x = p
            match f x with
            |Some(x)->return x
            |None->return! fail
        }


module ParserTest =
    let number = 
        Parser.Monad(){
            let! ds = Parser.some(Parser.satisfy System.Char.IsDigit)
            return 
                ds
                |>List.fold(sprintf"%s%c")""
                |>int
        }
    let addOps = 
        ['+',(+);'-',(-)]
        |>Map.ofList
        |>Parser.mapTokens

    let mulOps = 
        ['*',(*)]
        |>Map.ofList
        |>Parser.mapTokens

    let rec addmul =
        (List.foldBack Parser.chain)[addOps;mulOps]elem
    and elem =
        [
            lazy(number)
            lazy(Parser.pack '(' addmul ')')
        ]
        |>Parser.choose


    let test = 
        "(2+2)*2"
        |>List.ofSeq
        |>Parser.run(Parser.terminated addmul)
        |>Option.map(fst)
        |>Option.defaultValue -1
        