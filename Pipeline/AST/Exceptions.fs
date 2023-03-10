namespace Pipeline.AST

type TypeMatchException(a:PFunOrData,b:PFunOrData) = 
    inherit System.Exception(sprintf "типы не совпадают: %A, %A" a b)

type CannotPerformException(expName:string,a:PFunOrData)  =
    inherit System.Exception(sprintf "невозможно произвести %s(%A)" expName a)

type WrongTypeException(expName:string,right:System.Type,wrong:System.Type) = 
    inherit System.Exception(sprintf "функция %s ожидала тип %A однако получила %A" expName right wrong)

type NoSeqException(structName:string,object:obj) = 
    inherit System.Exception(sprintf "%s невозможно применить на %A" structName object)

type SeqOfFunctionException() = 
    inherit System.Exception(sprintf "невозможно создать последовательность из функций")

type NotAFunctionException(f:PFunOrData,d:PFunOrData) = 
    inherit System.Exception(sprintf "невозможно применить %A к %A так как %A не является функцией" d f f)