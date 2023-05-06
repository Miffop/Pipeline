namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST

type LogicOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"&&"->11
        |"||"->12
        |"^^"->13
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"&"->Op ALU.LAnd
        |"|"->Op ALU.LOr
        |"^"->Op ALU.LXor
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        Apply(Apply(this.Nullari(op,strImage),l),r)