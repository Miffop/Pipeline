namespace Pipeline.AST


type StringImage = 
    {CodeRef:string ref;SI:int;EI:int}
    override this.ToString() = (!this.CodeRef).[this.SI..this.EI]

type ALU = 
    //Arithmetic
    |Add            = 0
    |Sub            = 1
    |Mul            = 2
    |Div            = 3
    |Mod            = 4
    //Comparison
    |Eql            = 5
    |Neq            = 6
    |Grt            = 7
    |Geq            = 8
    //Logic
    |LAnd           = 9
    |LOr            = 10
    |LXor           = 11
    //Bitwise
    |BAnd           = 12
    |BOr            = 13
    |BXor           = 14
    |Shl            = 15
    |Shr            = 16
    //Unary
    |Neg            = 17
    |LNot           = 18
    |BNot           = 19
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
    //Pair 0x01
    |PairMake       = 0x0100
    |PairFirst      = 0x0101
    |PairSecond     = 0x0102
    |PairMatch      = 0x0103
    |PairMap        = 0x0104
    |PairMap2       = 0x0104
    //List 0x02
    |ListMake       = 0x0200
    |ListHead       = 0x0201
    |ListTail       = 0x0202
    |ListMatch      = 0x0203
    |ListIsEmpty    = 0x0204
    |ListMap        = 0x0205
    |ListJoin       = 0x0206
    |ListBind       = 0x0207
    //Maybe 0x03
    |MaybeValue     = 0x0300
    |MaybeHasValue  = 0x0301
    |MaybeMake      = 0x0302
    |MaybeBind      = 0x0303
    |MaybeMatch     = 0x0304
    //IO 0x04
    |IOWrite        = 0x0400
    |IORead         = 0x0401
    |IOMake         = 0x0402
    |IOBind         = 0x0403

type Expression = 
    |Op of ALU
    |F of Function
    //Unlambda marker
    |T of Expression
    //No expression marker
    |NullExpression
    //Basic types
    |Pair of Expression*Expression
    |List of Expression list
    |Maybe of Expression option
    |RealWorld
    //Literal
    |Int of int
    |Float of float
    |String of string
    |Logic of bool
    |Term of string
    |Lambda of string*Expression
    |Apply of Expression*Expression
    |Error of System.Exception
    |ErrorInfo of System.Exception


module Execution = 
    let rec isOccuringFree term exp = 
        match exp with
        |Term(x)        when x = term   -> true
        |Apply(x,y)                     -> isOccuringFree term x || isOccuringFree term y
        |Lambda(x,e)    when x <> term  -> isOccuringFree term e
        |T(e) -> isOccuringFree term e
        //|Pair(e1,e2) -> isOccuringFree term e1 || isOccuringFree term e2
        //|List(el)->List.exists(isOccuringFree term)el
        //|Maybe(Some(e))-> isOccuringFree term e
        |_->false
    let rec reduceT exp = 
        match exp with
        |T(Lambda(x,Apply(e,Term(y))))      when not(isOccuringFree x e) && x = y           -> e
        |T(Lambda(x,e))                     when not(isOccuringFree x e)                    -> Apply(F(Function.K),e)
        |T(Lambda(x,Term(y)))               when x=y                                        -> F(Function.I)
        |T(Lambda(x,Lambda(y,e)))                                                           -> T(Lambda(x,T(Lambda(y,e))))
        |T(Lambda(x,Apply(e1,e2)))          when isOccuringFree x e1 && isOccuringFree x e2 -> Apply(Apply(F(Function.S),T(Lambda(x,e1))),T(Lambda(x,e2)))
        |T(Lambda(x,Apply(e1,e2)))          when isOccuringFree x e1                        -> Apply(Apply(F(Function.C),T(Lambda(x,e1))),e2)
        |T(Lambda(x,Apply(e1,e2)))          when isOccuringFree x e2                        -> Apply(Apply(F(Function.B),e1),T(Lambda(x,e2)))
        |T(Lambda(x,T(e)))                                                                  -> T(Lambda(x,reduceT(T(e))))
        |T(Apply(x,y))                                                                      -> Apply(T(x),T(y))
        |x->x
    let rec reduce (exp:Expression) = 
        match exp with
        //Arithmetic
        |Apply(Apply(Op(ALU.Add),   Int(x)),    Int(y))->   Int(x+y)
        |Apply(Apply(Op(ALU.Add),   Float(x)),  Float(y))-> Float(x+y)
        |Apply(Apply(Op(ALU.Add),   String(x)), String(y))->String(x+y)
        |Apply(Apply(Op(ALU.Sub),   Int(x)),    Int(y))->   Int(x-y)
        |Apply(Apply(Op(ALU.Sub),   Float(x)),  Float(y))-> Float(x-y)
        |Apply(Apply(Op(ALU.Mul),   Int(x)),    Int(y))->   Int(x*y)
        |Apply(Apply(Op(ALU.Mul),   Float(x)),  Float(y))-> Float(x*y)
        |Apply(Apply(Op(ALU.Div),   Int(x)),    Int(y))     when y = 0->Error(System.DivideByZeroException())
        |Apply(Apply(Op(ALU.Div),   Float(x)),  Float(y))   when y = 0->Error(System.DivideByZeroException())
        |Apply(Apply(Op(ALU.Div),   Int(x)),    Int(y))->   Int(x/y)
        |Apply(Apply(Op(ALU.Div),   Float(x)),  Float(y))-> Float(x/y)
        |Apply(Apply(Op(ALU.Mod),   Int(x)),    Int(y))     when y = 0->Error(System.DivideByZeroException())
        |Apply(Apply(Op(ALU.Mod),   Float(x)),  Float(y))   when y = 0->Error(System.DivideByZeroException())
        |Apply(Apply(Op(ALU.Mod),   Int(x)),    Int(y))->   Int(x%y)
        |Apply(Apply(Op(ALU.Mod),   Float(x)),  Float(y))-> Float(x%y)
        //Unary
        |Apply(Op(ALU.Neg),         Int(x))->               Int(-x)
        |Apply(Op(ALU.Neg),         Float(x))->             Float(-x)
        |Apply(Op(ALU.LNot),        Logic(x))->             Logic(not x)
        |Apply(Op(ALU.LNot),        Int(x))->               Logic(not (x<>0))
        |Apply(Op(ALU.BNot),        Int(x))->               Int(~~~x)
        //Comparison
        |Apply(Apply(Op(ALU.Eql),   Int(x)),    Int(y))->   Logic(x=y)
        |Apply(Apply(Op(ALU.Eql),   Float(x)),  Float(y))-> Logic(x=y)
        |Apply(Apply(Op(ALU.Eql),   String(x)), String(y))->Logic(x=y)
        |Apply(Apply(Op(ALU.Eql),   Logic(x)),  Logic(y))-> Logic(x=y)
        |Apply(Apply(Op(ALU.Neq),   Int(x)),    Int(y))->   Logic(x<>y)
        |Apply(Apply(Op(ALU.Neq),   Float(x)),  Float(y))-> Logic(x<>y)
        |Apply(Apply(Op(ALU.Neq),   String(x)), String(y))->Logic(x<>y)
        |Apply(Apply(Op(ALU.Neq),   Logic(x)),  Logic(y))-> Logic(x<>y)
        |Apply(Apply(Op(ALU.Grt),   Int(x)),    Int(y))->   Logic(x>y)
        |Apply(Apply(Op(ALU.Grt),   Float(x)),  Float(y))-> Logic(x>y)
        |Apply(Apply(Op(ALU.Grt),   String(x)), String(y))->Logic(x>y)
        |Apply(Apply(Op(ALU.Grt),   Logic(x)),  Logic(y))-> Logic(x>y)
        |Apply(Apply(Op(ALU.Geq),   Int(x)),    Int(y))->   Logic(x>=y)
        |Apply(Apply(Op(ALU.Geq),   Float(x)),  Float(y))-> Logic(x>=y)
        |Apply(Apply(Op(ALU.Geq),   String(x)), String(y))->Logic(x>=y)
        |Apply(Apply(Op(ALU.Geq),   Logic(x)),  Logic(y))-> Logic(x>=y)
        //Logic
        |Apply(Apply(Op(ALU.LAnd),  Int(x)),    Int(y))->   Logic(x<>0 && y<>0)
        |Apply(Apply(Op(ALU.LAnd),  Logic(x)),  Logic(y))-> Logic(x&&y)
        |Apply(Apply(Op(ALU.LOr),   Int(x)),    Int(y))->   Logic(x<>0 || y<>0)
        |Apply(Apply(Op(ALU.LOr),   Logic(x)),  Logic(y))-> Logic(x||y)
        |Apply(Apply(Op(ALU.LXor),  Int(x)),    Int(y))->   Logic((x<>0 || y<>0) && not(x<>0 && y<>0))
        |Apply(Apply(Op(ALU.LXor),  Logic(x)),  Logic(y))-> Logic((x||y)&&not(x&&y))
        |Apply(Apply(Logic(true), x),y)->x
        |Apply(Apply(Logic(false),x),y)->y
        //Bitwise
        |Apply(Apply(Op(ALU.BAnd),  Int(x)),    Int(y))->   Int(x &&& y)
        |Apply(Apply(Op(ALU.BOr),   Int(x)),    Int(y))->   Int(x ||| y)
        |Apply(Apply(Op(ALU.BXor),  Int(x)),    Int(y))->   Int(x ^^^ y)
        |Apply(Apply(Op(ALU.Shl),   Int(x)),    Int(y))->   Int(x <<< y)
        |Apply(Apply(Op(ALU.Shr),   Int(x)),    Int(y))->   Int(x >>> y)
        //error handling
        |Apply(Apply(F(Function.Try),Error(e)),f)->                                  Apply(f,ErrorInfo(e))
        |Apply(Apply(F(Function.Try),x),f)->                                         x
        |Apply(F(Function.Throw),ErrorInfo(e))->                                     Error(e)
        |Apply(_,Error(e))->                                                         Error(e)
        |Apply(Error(e),_)->                                                         Error(e)
        //Pair
        |Apply(Apply(F(Function.PairMake),x),y)->                                    Pair(x,y)
        |Apply(F(Function.PairFirst),Pair(x,_))->                                    x
        |Apply(F(Function.PairSecond),Pair(_,y))->                                   y
        |Apply(Apply(F(Function.PairMatch),f),Pair(x,y))->                           Apply(Apply(f,x),y)
        |Apply(Apply(F(Function.PairMap),f),Pair(x,y))->                             Pair(Apply(f,x),Apply(f,y))
        |Apply(Apply(Apply(F(Function.PairMap2),f),Pair(x1,y1)),Pair(x2,y2))->       Pair(Apply(Apply(f,x1),x2),Apply(Apply(f,y1),y2))
        //List
        |Apply(Apply(F(Function.ListMake),x),List(l))->                              List(x::l)
        |Apply(F(Function.ListIsEmpty),List(l))->                                    Logic(List.isEmpty l)
        |Apply(F(Function.ListHead),List([]))->                                      Error(System.Exception("У пустого списка нет головы"))
        |Apply(F(Function.ListHead),List(x::_))->                                    x
        |Apply(F(Function.ListTail),List([]))->                                      Error(System.Exception("У пустого списка нет хвоста"))
        |Apply(F(Function.ListTail),List(_::y))->                                    List(y)
        |Apply(Apply(F(Function.ListMap),f),List(l))->                               List(List.map(fun x->Apply(f,x))l)
        |Apply(F(Function.ListJoin),List(ls)) when List.forall(function List(l)->true|_->false)ls-> 
                                                                                     List(List.collect(function List(l)->l)ls)
        |Apply(Apply(F(Function.ListBind),List(Ma)),f)->                             Apply(F(Function.ListJoin),Apply(Apply(F(Function.ListMap),f),List(Ma)))
        |Apply(Apply(Apply(F(Function.ListMatch),f1),f2),List([])) ->                f2
        |Apply(Apply(Apply(F(Function.ListMatch),f1),f2),List(x::y)) ->              Apply(Apply(f1,x),List(y))
        
        //Maybe
        |Apply(F(Function.MaybeMake),x)->                                            Maybe(Some(x))
        |Apply(F(Function.MaybeValue),Maybe(None))->                                 Error(System.Exception("Указатель пустой"))
        |Apply(F(Function.MaybeValue),Maybe(Some(x)))->                              x
        |Apply(F(Function.MaybeHasValue),Maybe(x))->                                 Logic(Option.isSome x)
        |Apply(Apply(F(Function.MaybeBind),Maybe(None)),_)->                         Maybe(None)
        |Apply(Apply(F(Function.MaybeBind),Maybe(Some(a))),f)->                      Apply(f,a)
        |Apply(Apply(Apply(F(Function.MaybeMatch),f1),f2),Maybe(None)) ->            f2
        |Apply(Apply(Apply(F(Function.MaybeMatch),f1),f2),Maybe(Some(x))) ->         Apply(f1,x)
        //IO
        |Apply(Apply(F(Function.IOWrite),String(x)),RealWorld)->                     printfn "%s" x
                                                                                     Pair(RealWorld,F(Function.I))
        |Apply(F(Function.IORead),RealWorld) ->                                      Pair(RealWorld,String(System.Console.ReadLine()))
        |Apply(Apply(F(Function.IOMake),x),RealWorld)->                              Pair(RealWorld,x)
        |Apply(Apply(Apply(F(Function.IOBind),Ma),f),RealWorld)->                    Apply(Apply(f,Apply(F(Function.PairSecond),Apply(Ma,RealWorld))),RealWorld)

        //Combinator calculus
        |Apply(F(Function.I),x)->                           x
        |Apply(Apply(F(Function.K),x),_)->x
        |Apply(Apply(Apply(F(Function.S),x),y),z)->         Apply(Apply(x,z),Apply(y,z))
        |Apply(Apply(Apply(F(Function.B),x),y),z)->         Apply(x,Apply(y,z))
        |Apply(Apply(Apply(F(Function.C),x),y),z)->         Apply(Apply(x,z),y)
        |Apply(Apply(F(Function.Y),f),x)->                  Apply(Apply(f,Apply(F(Function.Y),f)),x)
        //Lambda -> Combinator translation
        |Lambda(x,e)->                                      T(Lambda(x,e))
        |T(x)->                                             reduceT(T(x))
        //Going deeper with reductions
        |Apply(x,y)->                                       Apply(reduce x,reduce y)
        |List(l)->                                          List(List.map reduce l)
        |Pair(x,y)->                                        Pair(reduce x,reduce y)
        |Maybe(Some(x))->                                   Maybe(Some(reduce x))
        //No reduction
        |x->x
    let execution = 
        Seq.unfold(fun x->let y = reduce x in if x=y then None else Some(y,y))
        
        