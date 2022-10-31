namespace Pipeline.AST

type TypeMatchException(a:PFunOrData,b:PFunOrData) = 
    inherit System.Exception()
    override this.Message = sprintf "types don't match: %A, %A" a b
type CannotPerformException(expName:string,a:PFunOrData)  =
    inherit System.Exception()
    override this.Message = sprintf "cannot perform %s(%A)" expName a
type WrongTypeException(expName:string,right:System.Type,wrong:System.Type) = 
    inherit System.Exception()
    override this.Message = sprintf "%s expected to have type %A but has type %A" expName right wrong
type NoSeqException(structName:string,object:obj) = 
    inherit System.Exception()
    override this.Message = sprintf "%s cannot be performed on %A" structName object
type SeqOfFunctionException() = 
    inherit System.Exception()
    override this.Message = sprintf "cannot create seq of functions"
type NotAFunctionException(f:PFunOrData,d:PFunOrData) = 
    inherit System.Exception()
    override this.Message = sprintf "cannot apply %A to %A because %A is not a function" d f f