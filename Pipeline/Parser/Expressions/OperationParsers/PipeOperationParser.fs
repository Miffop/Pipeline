namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.AST.Funcs
open Pipeline.AST.Expressions
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

type PipeOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"|>"|"|!>"
            ->20
        |_->(-1)
    override this.Nullari(op) = 
        raise<|System.NotImplementedException()
    override this.UnaryLeft(op,l) =
        match op.Content with
        |"|>"->raise<|OperationIsNotUnary(op.Content)
        |"|!>"->l
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r) = 
        this.UnaryLeft(op,r)
    override this.Binary(op,l,r) = 
        match op.Content with
        |"|>"->PipeExpression(false,l,r)
        |"|!>"->PipeExpression(true,l,r)
        |_->raise<|System.NotImplementedException()