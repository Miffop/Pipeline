namespace Pipeline.Parser.Expressions


type UnknownOperationExpression(name:string) = 
    inherit System.Exception(sprintf "неизвесная операция %s" name)

type OperationIsNotBinary(name:string) = 
    inherit System.Exception(sprintf "операция %s операция не является бинарной" name)

type OperationIsNotUnary(name:string) = 
    inherit System.Exception(sprintf "операция %s не является унарной" name)

type OperationIsNotUnaryLeft(name:string) = 
    inherit System.Exception(sprintf "унарная операция %s не принимает операнд слева" name)

type OperationIsNotUnaryRight(name:string) = 
    inherit System.Exception(sprintf "унарная операция %s не принимает операнд справа" name)
