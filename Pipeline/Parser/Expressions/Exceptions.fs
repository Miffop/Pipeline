namespace Pipeline.Parser.Expressions


type UnknownOperationExpression(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "неизвесная операция %s" name
    
type OperationIsNotBinary(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "операция %s операция не является бинарной" name
type OperationIsNotUnary(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "операция %s не является унарной" name
type OperationIsNotUnaryLeft(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "унарная операция %s не принимает операнд слева" name
type OperationIsNotUnaryRight(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "унарная операция %s не принимает операнд справа" name
