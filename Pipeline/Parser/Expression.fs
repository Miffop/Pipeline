namespace Pipeline.Parser

open Pipeline.AST

module ExpressionParser =
    let private op f x y = Apply(Apply(F f,x),y)
    let private newOp = Map.ofList>>Indentation.mapTokens
    let private flip f x y = f y x
    
    let word = Indentation.bindOption(function |Token.Word(x)->Some(x)|_->None)Parser.one

    module Operations = 
        let addOps = 
            [
                Token.Op("+"),op Function.Add
                Token.Op("-"),op Function.Sub
            ]|>newOp
        let mulOps = 
            [
                Token.Op("*"),op Function.Mul
                Token.Op("/"),op Function.Div
            ]|>newOp
        let compOps = 
            [
                Token.Op("болш"),       op Function.Grt
                Token.Op("менш"),       op Function.Grt |> flip
                Token.Op("болшр"),      op Function.Geq
                Token.Op("меншр"),      op Function.Geq |> flip
                Token.Op("="),          op Function.Eql
                Token.Op("!="),         op Function.Neq
            ]|>newOp
        let logicOps = 
            [
                Token.Op("или"),        op Function.LOr
                Token.Op("и"),          op Function.LAnd
            ]|>newOp
        let flowOps =
            let BBB = Apply(Apply(F Function.B,F Function.B),F Function.B)
            [
                Token.Op("."),          (fun x y ->Apply(y,x))
                Token.Op(".."),         op Function.B |> flip
                Token.Op("..."),        (fun x y ->Apply(Apply(BBB,y),x))
                Token.Op(","),          (fun x y ->Apply(x,y))
                Token.Op(",,"),         op Function.B
                Token.Op(",,,"),        (fun x y ->Apply(Apply(BBB,x),y))

            ]|>newOp
        
        let order opl opr= 
            [
                Indentation.chain flowOps
                Indentation.chain logicOps
                Indentation.chain compOps
                Indentation.chain addOps
                Indentation.chain mulOps
                Indentation.chain(Indentation.ret(fun x y->Apply(x,y)))
                
                Indentation.chainBack opr
                Indentation.chain opl
            ]
    module Functions = 
        let Ops = 
            let flip f = Apply(F Function.C, f)
            [
                "+",            F Function.Add
                "-",            F Function.Sub
                "*",            F Function.Mul
                "/",            F Function.Div
                "болш",         F Function.Grt
                "менш",         F Function.Grt  |> flip
                "болшр",        F Function.Geq
                "меншр",        F Function.Geq  |> flip
                "=",            F Function.Eql
                "!=",           F Function.Neq
                "или",          F Function.LOr
                "и",            F Function.LAnd
                ",",            F Function.I
                ",,",           F Function.B
                ",,,",          Apply(Apply(F Function.B,F Function.B),F Function.B)
                ".",            F Function.I    |> flip
                "..",           F Function.B    |> flip
                "...",          Apply(Apply(F Function.B,F Function.B),F Function.B) |> flip
                "если",         F Function.If
            ]
        let Funcs = 
            [
                "не",           F Function.LNot
                "пов",          F Function.C
            ]
            |>Map.ofList
    
    let rec Expression = 
        let myLeft = 
            Indentation.Monad(){
                let! _ = Indentation.token(Token.Op "\\\\")
                let! e = Element
                return (fun x y -> Apply(Apply(e,x),y))
            }
        let myRight =
            Indentation.Monad(){
                let! _ = Indentation.token(Token.Op "//")
                let! e = Element
                return (fun x y -> Apply(Apply(e,x),y))
            }
        
        [
            lazy(List.foldBack(fun x y -> x y)(Operations.order myLeft myRight) Element)
        ]
        |>Parser.choose
        |>Indentation.sameOrIndentedScope Token.BraceOpen Token.BraceClose
    and Func = 
        let rec lambdas ps b = 
            match ps with
            |p::ps -> lambdas ps (Lambda(p,b))
            |[]->b
        let SemiFunc = 
            let letters = ['а'..'е']|>List.map(string)
            let lettersBind x =
                match x with
                |x when List.length x > List.length letters -> None
                |x -> Some<|List.take(List.length x)letters
            Indentation.Monad(){
                let! parameters = Indentation.bindOption(lettersBind)<|Indentation.some(Indentation.token<|Token.Op ";")
                let! body = Expression
                return lambdas(List.rev parameters)body
            }
        let ParametaisedFunc = 
            Indentation.Monad(){
                let! parameters = Indentation.some(word)
                let! _ = Indentation.token(Token.Op ":")
                let! body = Expression
                return lambdas(List.rev parameters)body
            }
        let LetFunc = 
            Indentation.Monad(){
                let! _ = Indentation.token(Token.Op "пусть")
                let! recursion = Indentation.option(Indentation.token(Token.Op "рек"))
                let! name::paraments = Indentation.some(word)
                let! _ = Indentation.token(Token.Op "=")
                let! body = Expression
                let! next = Expression
                let func = 
                    lambdas(List.rev paraments)body
                    |>match recursion with
                        |None -> (fun x->x)
                        |_->(fun x->Apply(F Function.Y,Lambda(name,x)))

                return Apply(Lambda(name,next),func)
            }
        [
            lazy(ParametaisedFunc)
            lazy(SemiFunc)
            lazy(LetFunc)
        ]
        |>Parser.choose
    and Cond = 
        (*let subCond = 
            Parser.Monad(){
                let! _ = Parser.token(Token.Word "иначе")
                let! _ = Parser.token(Token.Word "если")
                let! c = Expression
                let! _ = Parser.token(Token.Word "то")
                let! a = Expression
               return (fun b -> Apply(Apply(c,a),b))
            }*)
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "если")
            let! c = Expression
            let! _ = Indentation.token(Token.Op "то")
            let! a = Expression
            let! _ = Indentation.token(Token.Op "иначе")
            let! b = Expression
            return Apply(Apply(Apply(F Function.If,c),a),b)
        }
    and Let = 
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "пусть")
            let! binding = word
            let! _ = Indentation.token(Token.Op "=")
            let! body = Expression
            let! next = Expression
            return Apply(Lambda(binding,next),body)
        }

    and Element =
        let literal t = 
            match t with
            |Token.Int x        ->Some<|L(Int x)
            |Token.Float x      ->Some<|L(Float x)
            |Token.String  x    ->Some<|L(String x)
            |_->None
        let term =
            function
            |Token.Word x       when Map.containsKey x Functions.Funcs
                                ->Some<|Functions.Funcs[x]
            |Token.Word x       ->Some<|Term x
            |_->None
        let ops = 
            Functions.Ops
            |>List.map(fun (x,y)->Token.Op x,y)
            |>Map.ofList
            |>Indentation.mapTokens
        [
            lazy(Indentation.bindOption literal Parser.one)
            lazy(Indentation.prefix (Token.Op "\\") ops)
            lazy(Indentation.bindOption term Parser.one)
            
            lazy(Cond)
            lazy(Func)
            lazy(Let)
            lazy(Indentation.packLazy Token.BraceOpen (lazy(Expression)) Token.BraceClose)
        ]
        |>Parser.choose