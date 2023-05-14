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
        |"="->F Function.Eql
        |"!="->F Function.Neq
        |">"->F Function.Grt
        |"<"->Apply(F Function.C,F Function.Grt)
        |">="->F Function.Geq
        |"<="->Apply(F Function.C,F Function.Geq)
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        Apply(Apply(this.Nullari(op,strImage),l),r)