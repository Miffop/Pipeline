namespace Pipeline.Parser.Expressions


type UnknownOperationExpression(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "unknown operation %s" name
    
type OperationIsNotBinary(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "operation %s is not binary" name
type OperationIsNotUnary(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "operation %s is not unary" name
type OperationIsNotUnaryLeft(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "unary operation %s do not support left operand" name
type OperationIsNotUnaryRight(name:string) = 
    inherit System.Exception()
    override this.Message = sprintf "unary operation %s do not support right operand" name
