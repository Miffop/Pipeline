namespace Pipeline.AST


type StringImage = 
    {CodeRef:string ref;SI:int;EI:int}
    override this.ToString() = (!this.CodeRef).[this.SI..this.EI]

type Function =
    //Basic 0x00
    |Y              = 0x0000
    |S              = 0x0001
    |K              = 0x0002
    |I              = 0x0003
    |B              = 0x0004
    |C              = 0x0005
    |Try            = 0x0006
    |Throw          = 0x0007
    //Maths 0x01
    (* 0x010 Arithmetic *)
    |Add            = 0x0100
    |Sub            = 0x0101
    |Mul            = 0x0102
    |Div            = 0x0103
    |Mod            = 0x0104
    |Neg            = 0x0105
    (* 0x011 Comparison *)
    |Eql            = 0x0110
    |Neq            = 0x0111
    |Grt            = 0x0112
    |Geq            = 0x0113
    (* 0x012 Logic *)
    |LAnd           = 0x0120
    |LOr            = 0x0121
    |LXor           = 0x0122
    |LNot           = 0x0123
    |If             = 0x0124
    (* 0x013 Bitwise operations *)
    |BAnd           = 0x0130
    |BOr            = 0x0131
    |BXor           = 0x0132
    |Shl            = 0x0133
    |Shr            = 0x0134
    |BNot           = 0x0135
    //Pair 0x02
    |PairMake       = 0x0200
    |PairFirst      = 0x0201
    |PairSecond     = 0x0202
    |PairMatch      = 0x0203
    |PairMap        = 0x0204
    |PairMap2       = 0x0204
    //List 0x03
    |ListMake       = 0x0300
    |ListHead       = 0x0301
    |ListTail       = 0x0302
    |ListMatch      = 0x0303
    |ListIsEmpty    = 0x0304
    |ListMap        = 0x0305
    |ListJoin       = 0x0306
    |ListBind       = 0x0307
    //Maybe 0x04
    |MaybeValue     = 0x0400
    |MaybeHasValue  = 0x0401
    |MaybeMake      = 0x0402
    |MaybeBind      = 0x0403
    |MaybeMatch     = 0x0404
    //IO 0x05
    |IOWrite        = 0x0500
    |IORead         = 0x0501
    |IOMake         = 0x0502
    |IOBind         = 0x0503

type Literal = 
    |Int of int
    |Float of float
    |String of string
    |Logic of bool
    |ErrorInfo of System.Exception
    override this.ToString() = 
        match this with
        |Int    i       -> sprintf "%i" i
        |Float  f       -> sprintf "%f" f
        |String s       -> sprintf "\"%s\""s
        |Logic  true    -> "@t"
        |Logic  false   -> "@f"
        |ErrorInfo e    -> sprintf "@errorInfo"


type Expression = 
    |F of Function
    //unlambda marker
    |T of Expression
    //No expression marker
    |NullExpression
    //Basic types
    |Pair of Expression*Expression
    |List of Expression list
    |Maybe of Expression option
    |RealWorld
    //Literal
    |L of Literal
    |Term of string
    |Lambda of string*Expression
    |Apply of Expression*Expression
    |Error of System.Exception
    override this.ToString() = 
        match this with
        //constants
        |F f            -> sprintf "%A" f
        |NullExpression -> sprintf "@parserNull"
        |L(l)           -> sprintf "%O" l
        //expressions
        |Term(t)        -> sprintf "%s" t
        |Lambda(x,e)    -> sprintf "(\%s.%O)" x e
        |Apply(e1,Apply(e2,e3))
                        -> sprintf "%O(%O)" e1<|Apply(e2,e3)
        |Apply(e1,e2)   -> sprintf "%O %O" e1 e2
        //Data structs
        |Pair(_,_)      -> sprintf "@pair"
        |List(_)        -> sprintf "@list"
        |Maybe(_)       -> sprintf "@maybe"
        |RealWorld      -> sprintf "@rw"
        //errors
        |Error(e)       -> sprintf "@error"
        |T(x)           -> sprintf "T[%O]" x
        
module Execution = 
    
    let rec reduce matcher = 
        [
            matcher
            M(
                function
                |Apply(x,y)->                                       Some<|Apply(reduce matcher x,reduce matcher y)
                |List(l)->                                          Some<|List(List.map (reduce matcher) l )
                |Pair(x,y)->                                        Some<|Pair(reduce matcher x,reduce matcher y)
                |Maybe(Some(x))->                                   Some<|Maybe(Some(reduce matcher x))
                |_->None
            )
        ]
        |>Match.choose
        |>Match.extendWith(fun x->x)
        
    
    
    let execution matcher a = 
        Seq.unfold(fun x->let y = reduce matcher x in if x=y then None else Some(y,y)) a
        |>Seq.insertAt 0 a
        