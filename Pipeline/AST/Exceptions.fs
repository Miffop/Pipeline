namespace Pipeline.AST

type TypeMatchException(a:PFunOrData,b:PFunOrData) = 
    inherit System.Exception()
    override this.Message = sprintf "types don't match: %A, %A" a b
type CannotPerformException(expName:string,a:PFunOrData)  =
    inherit System.Exception()
    override this.Message = sprintf "cannot perform %s(%A)" expName a
type NoSeqException(structName:string,object:obj) = 
    inherit System.Exception()
    override this.Message = sprintf "%s cannot be performed on %A" structName object
type SeqOfFunctionException() = 
    inherit System.Exception()
    override this.Message = sprintf "cannot create seq of functions"