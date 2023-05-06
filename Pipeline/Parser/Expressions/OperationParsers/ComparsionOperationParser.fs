namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST

type ComparisonOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"="
        |"!="
        |">"
        |">="
        |"<"
        |"<="
            ->10
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"="->Op ALU.Eql
        |"!="->Op ALU.Neq
        |">"->Op ALU.Grt
        |"<"->Apply(F Function.C,Op ALU.Grt)
        |">="->Op ALU.Geq
        |"<="->Apply(F Function.C,Op ALU.Geq)
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        Apply(Apply(this.Nullari(op,strImage),l),r)