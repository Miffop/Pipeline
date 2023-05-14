namespace Pipeline.AST


type Matcher<'a,'b> = 
    |M of ('a -> 'b option)

type OptionMonad() = 
    member this.Return x = Some x
    member this.ReturnFrom x = x
    member this.Bind(Ma,f) = 
        match Ma with
        |Some(a)->f a
        |None->None

module Match = 
    let fail = M(fun _ -> None)
    let identity = M(fun x->Some(x))
    let run m a = 
        match m with
        |M(m) -> m a
    let runUnsafe m a =
        match run m a with
        |Some(x)->x
        |None->failwith "результат был пуст"
    let choose ms = 
        let (<|>) m1 m2 = M(fun a->
            match run m1 a with
            |None->run m2 a
            |x->x
        )
        List.reduce(<|>)ms
    let combine m1 m2 = M(fun a->
            OptionMonad(){
                let! a1 = run m1 a
                return! run m2 a1
            }
        )
    let concat ms = List.reduce(combine)ms
    let extendWith f m = (fun a->
        match run m a with
        |Some(a)->a
        |None->f a
    )
    let mapIn f m = M(fun a->run m (f a))
    let mapOut f m = M(fun a->
        OptionMonad(){
            let! x = run m a
            return f x
        }
    )
    
        
        
    