namespace Pipeline.AST

module Reductions = 
    
    let errorHandling = M(
        function
        |Apply(Apply(F(Function.Try),Error(e)),f)->                Some<|Apply(f,L(ErrorInfo e))
        |Apply(Apply(F(Function.Try),x),f)->                       Some<|x
        |Apply(F(Function.Throw),L(ErrorInfo e))->                 Some<|Error(e)
        |Apply(_,Error(e))->                                       Some<|Error(e)
        |Apply(Error(e),_)->                                       Some<|Error(e)
        |_->None
    )
    let combinators = M(
        function
        |Apply(F(Function.I),x)->                           Some<|x
        |Apply(Apply(F(Function.K),x),_)->                  Some<|x
        |Apply(Apply(Apply(F(Function.S),x),y),z)->         Some<|Apply(Apply(x,z),Apply(y,z))
        |Apply(Apply(Apply(F(Function.B),x),y),z)->         Some<|Apply(x,Apply(y,z))
        |Apply(Apply(Apply(F(Function.C),x),y),z)->         Some<|Apply(Apply(x,z),y)
        |Apply(Apply(F(Function.Y),f),x)->                  Some<|Apply(Apply(f,Apply(F(Function.Y),f)),x)
        |_->None
    )
    let unlambda = 
        let rec isOccuringFree term exp = 
            match exp with
            |Term(x)        when x = term   -> true
            |Apply(x,y)                     -> isOccuringFree term x || isOccuringFree term y
            |Lambda(x,e)    when x <> term  -> isOccuringFree term e
            //|T(e) -> isOccuringFree term e
            //|Pair(e1,e2) -> isOccuringFree term e1 || isOccuringFree term e2
            //|List(el)->List.exists(isOccuringFree term)el
            //|Maybe(Some(e))-> isOccuringFree term e
            |_->false
        let rec reduceT exp = 
            match exp with
            |Lambda(x,Apply(e,Term(y)))      when not(isOccuringFree x e) && x = y           -> e
            |Lambda(x,e)                     when not(isOccuringFree x e)                    -> Apply(F(Function.K),e)
            |Lambda(x,Term(y))               when x=y                                        -> F(Function.I)
            |Lambda(x,Apply(e1,e2))          when isOccuringFree x e1 && isOccuringFree x e2 -> Apply(Apply(F(Function.S),Lambda(x,e1)) ,Lambda(x,e2))
            |Lambda(x,Apply(e1,e2))          when isOccuringFree x e1                        -> Apply(Apply(F(Function.C),Lambda(x,e1)) ,e2)
            |Lambda(x,Apply(e1,e2))          when isOccuringFree x e2                        -> Apply(Apply(F(Function.B),e1)           ,Lambda(x,e2))
            |Lambda(x,e)                                                                     -> Lambda(x,reduceT(e))
            |x->x
        M(
            function
            |Lambda(x,e)->                                         Some<|reduceT(Lambda(x,e))
            |_->None
        )
    module Maths = 
        let arithmetic = M(
            function
            |Apply(Apply(F Function.Add,    L(Int       x)),    L(Int       y))->   Some<|L(Int(x+y))
            |Apply(Apply(F Function.Add,    L(Float     x)),    L(Float     y))->   Some<|L(Float(x+y))
            |Apply(Apply(F Function.Add,    L(String    x)),    L(String    y))->   Some<|L(String(x+y))
            |Apply(Apply(F Function.Sub,    L(Int       x)),    L(Int       y))->   Some<|L(Int(x-y))
            |Apply(Apply(F Function.Sub,    L(Float     x)),    L(Float     y))->   Some<|L(Float(x-y))
            |Apply(Apply(F Function.Mul,    L(Int       x)),    L(Int       y))->   Some<|L(Int(x*y))
            |Apply(Apply(F Function.Mul,    L(Float     x)),    L(Float     y))->   Some<|L(Float(x*y))
            |Apply(Apply(F Function.Div,    L(Int       x)),    L(Int       y))     when y = 0->Some<|Error(System.DivideByZeroException())
            |Apply(Apply(F Function.Div,    L(Float     x)),    L(Float     y))     when y = 0->Some<|Error(System.DivideByZeroException())
            |Apply(Apply(F Function.Div,    L(Int       x)),    L(Int       y))->   Some<|L(Int(x/y))
            |Apply(Apply(F Function.Div,    L(Float     x)),    L(Float     y))->   Some<|L(Float(x/y))
            |Apply(Apply(F Function.Mod,    L(Int       x)),    L(Int       y))     when y = 0->Some<|Error(System.DivideByZeroException())
            |Apply(Apply(F Function.Mod,    L(Float     x)),    L(Float     y))     when y = 0->Some<|Error(System.DivideByZeroException())
            |Apply(Apply(F Function.Mod,    L(Int       x)),    L(Int       y))->   Some<|L(Int(x%y))
            |Apply(Apply(F Function.Mod,    L(Float     x)),    L(Float     y))->   Some<|L(Float(x%y))
            //Unary
            |Apply(      F Function.Neg,   L(Int    x))->                       Some<|L(Int(-x))
            |Apply(      F Function.Neg,   L(Float  x))->                       Some<|L(Float(-x))
            |_->None
        )
        let comparison = M(
            function
            |Apply(Apply(F Function.Eql,    L(Int       x)),    L(Int       y))->   Some<|Logic(x=y)
            |Apply(Apply(F Function.Eql,    L(Float     x)),    L(Float     y))->   Some<|Logic(x=y)
            |Apply(Apply(F Function.Eql,    L(String    x)),    L(String    y))->   Some<|Logic(x=y)
            |Apply(Apply(F Function.Eql,    L(Logic     x)),    L(Logic     y))->   Some<|Logic(x=y)
            |Apply(Apply(F Function.Neq,    L(Int       x)),    L(Int       y))->   Some<|Logic(x<>y)
            |Apply(Apply(F Function.Neq,    L(Float     x)),    L(Float     y))->   Some<|Logic(x<>y)
            |Apply(Apply(F Function.Neq,    L(String    x)),    L(String    y))->   Some<|Logic(x<>y)
            |Apply(Apply(F Function.Neq,    L(Logic     x)),    L(Logic     y))->   Some<|Logic(x<>y)
            |Apply(Apply(F Function.Grt,    L(Int       x)),    L(Int       y))->   Some<|Logic(x>y)
            |Apply(Apply(F Function.Grt,    L(Float     x)),    L(Float     y))->   Some<|Logic(x>y)
            |Apply(Apply(F Function.Grt,    L(String    x)),    L(String    y))->   Some<|Logic(x>y)
            |Apply(Apply(F Function.Grt,    L(Logic     x)),    L(Logic     y))->   Some<|Logic(x>y)
            |Apply(Apply(F Function.Geq,    L(Int       x)),    L(Int       y))->   Some<|Logic(x>=y)
            |Apply(Apply(F Function.Geq,    L(Float     x)),    L(Float     y))->   Some<|Logic(x>=y)
            |Apply(Apply(F Function.Geq,    L(String    x)),    L(String    y))->   Some<|Logic(x>=y)
            |Apply(Apply(F Function.Geq,    L(Logic     x)),    L(Logic     y))->   Some<|Logic(x>=y)
            |_->None
            >>Option.map L
        )
        let logic = 
          [
           M(
            function
            |Apply(Apply(F Function.LAnd,   L(Int       x)),    L(Int       y))->   Some<|Logic(x<>0 && y<>0)
            |Apply(Apply(F Function.LAnd,   L(Logic     x)),    L(Logic     y))->   Some<|Logic(x&&y)
            |Apply(Apply(F Function.LOr,    L(Int       x)),    L(Int       y))->   Some<|Logic(x<>0 || y<>0)
            |Apply(Apply(F Function.LOr,    L(Logic     x)),    L(Logic     y))->   Some<|Logic(x||y)
            |Apply(Apply(F Function.LXor,   L(Int       x)),    L(Int       y))->   Some<|Logic((x<>0 || y<>0) && not(x<>0 && y<>0))
            |Apply(Apply(F Function.LXor,   L(Logic     x)),    L(Logic     y))->   Some<|Logic((x||y)&&not(x&&y))
            //Unary
            |Apply(      F Function.LNot,   L(Logic     x))->                       Some<|Logic(not x)
            |Apply(      F Function.LNot,   L(Int       x))->                       Some<|Logic(not (x<>0))
            //conditions
            |_->None
            >>Option.map L
           )
           M(
            function
            |Apply(Apply(L(Logic true), x),y)->Some<|x
            |Apply(Apply(L(Logic false),x),y)->Some<|y
            |_->None
           )
          ]
          |>Match.choose
        let bitwise = M(
            function
            |Apply(Apply(F Function.BAnd,   L(Int       x)),    L(Int       y))->   Some<|Int(x &&& y)
            |Apply(Apply(F Function.BOr,    L(Int       x)),    L(Int       y))->   Some<|Int(x ||| y)
            |Apply(Apply(F Function.BXor,   L(Int       x)),    L(Int       y))->   Some<|Int(x ^^^ y)
            |Apply(Apply(F Function.Shl,    L(Int       x)),    L(Int       y))->   Some<|Int(x <<< y)
            |Apply(Apply(F Function.Shr,    L(Int       x)),    L(Int       y))->   Some<|Int(x >>> y)
            //Unary
            |Apply(      F Function.BNot,  L(Int        x))->                       Some<|Int(~~~x)
            |_->None
            >>Option.map L
        )
        let errors = M(
            function
            |Apply(Apply(F op,L(x)),L(y)) when (int op &&& 0x0100) <> 0
                -> Some<|Error(System.Exception(sprintf "типы не сходятся: %A %A" x y))
            |_->None
        )
    module Data = 
        let pair = M(
            function
            |Apply(Apply(F(Function.PairMake),x),y)->                                    Some<|Pair(x,y)
            |Apply(F(Function.PairFirst),Pair(x,_))->                                    Some<|x
            |Apply(F(Function.PairSecond),Pair(_,y))->                                   Some<|y
            |Apply(Apply(F(Function.PairMatch),f),Pair(x,y))->                           Some<|Apply(Apply(f,x),y)
            |Apply(Apply(F(Function.PairMap),f),Pair(x,y))->                             Some<|Pair(Apply(f,x),Apply(f,y))
            |Apply(Apply(Apply(F(Function.PairMap2),f),Pair(x1,y1)),Pair(x2,y2))->       Some<|Pair(Apply(Apply(f,x1),x2),Apply(Apply(f,y1),y2))
            |_->None
        )
        let list = M(
            function
            |Apply(Apply(F(Function.ListMake),x),List(l))->                              Some<|List(x::l)
            |Apply(F(Function.ListIsEmpty),List(l))->                                    Some<|L(Logic(List.isEmpty l))
            |Apply(F(Function.ListHead),List([]))->                                      Some<|Error(System.Exception("У пустого списка нет головы"))
            |Apply(F(Function.ListHead),List(x::_))->                                    Some<|x
            |Apply(F(Function.ListTail),List([]))->                                      Some<|Error(System.Exception("У пустого списка нет хвоста"))
            |Apply(F(Function.ListTail),List(_::y))->                                    Some<|List(y)
            |Apply(Apply(F(Function.ListMap),f),List(l))->                               Some<|List(List.map(fun x->Apply(f,x))l)
            |Apply(F(Function.ListJoin),List(ls)) when List.forall(function List(l)->true|_->false)ls-> 
                                                                                         Some<|List(List.collect(function List(l)->l)ls)
            |Apply(Apply(F(Function.ListBind),List(Ma)),f)->                             Some<|Apply(F(Function.ListJoin),Apply(Apply(F(Function.ListMap),f),List(Ma)))
            |Apply(Apply(Apply(F(Function.ListMatch),f1),f2),List([])) ->                Some<|f2
            |Apply(Apply(Apply(F(Function.ListMatch),f1),f2),List(x::y)) ->              Some<|Apply(Apply(f1,x),List(y))
            |_->None
        )
        let maybe = M(
            function
            |Apply(F(Function.MaybeMake),x)->                                            Some<|Maybe(Some(x))
            |Apply(F(Function.MaybeValue),Maybe(None))->                                 Some<|Error(System.Exception("Указатель пустой"))
            |Apply(F(Function.MaybeValue),Maybe(Some(x)))->                              Some<|x
            |Apply(F(Function.MaybeHasValue),Maybe(x))->                                 Some<|L(Logic(Option.isSome x))
            |Apply(Apply(F(Function.MaybeBind),Maybe(None)),_)->                         Some<|Maybe(None)
            |Apply(Apply(F(Function.MaybeBind),Maybe(Some(a))),f)->                      Some<|Apply(f,a)
            |Apply(Apply(Apply(F(Function.MaybeMatch),f1),f2),Maybe(None)) ->            Some<|f2
            |Apply(Apply(Apply(F(Function.MaybeMatch),f1),f2),Maybe(Some(x))) ->         Some<|Apply(f1,x)
            |_->None
        )
        let IOMonad = M(
            function
            |Apply(Apply(F(Function.IOWrite),L(String x)),RealWorld)->                   printfn "%s" x
                                                                                         Some<|Pair(RealWorld,F(Function.I))
            |Apply(F(Function.IORead),RealWorld) ->                                      Some<|Pair(RealWorld,L(String<|System.Console.ReadLine()))
            |Apply(Apply(F(Function.IOMake),x),RealWorld)->                              Some<|Pair(RealWorld,x)
            |Apply(Apply(Apply(F(Function.IOBind),Ma),f),RealWorld)->                    Some<|Apply(Apply(f,Apply(F(Function.PairSecond),Apply(Ma,RealWorld))),RealWorld)
            |_->None
        )