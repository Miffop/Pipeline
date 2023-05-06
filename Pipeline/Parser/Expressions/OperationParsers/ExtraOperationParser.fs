namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST

type ExtraOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |","
            ->15
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |","->F Function.ListMake
        |_->raise<|System.NotImplementedException()
    override this.Binary(op,l,r,strImage) = 
        Apply(Apply(this.Nullari(op,strImage),l),r)
    override this.GetOrderDirection op= 
        match op.Content with
        |","
            ->false
        |_->raise<|System.NotImplementedException()