namespace Pipeline



type OptionMonad() = 
    member this.Return x = Some x
    member this.ReturnFrom x = x
    member this.Bind(Ma,f) = Option.bind f Ma


