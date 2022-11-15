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
    override this.Nullari(op,strImage) = 
        raise<|System.NotImplementedException()
    override this.UnaryLeft(op,l,strImage) =
        match op.Content with
        |"|>"->raise<|OperationIsNotUnary(op.Content)
        |"|!>"->l
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r,strImage) = 
        this.UnaryLeft(op,r,strImage)
    override this.Binary(op,l,r,strImage) = 
        match op.Content with
        |"|>"->PipeExpression(false,l,r,Some strImage)
        |"|!>"->PipeExpression(true,l,r,Some strImage)
        |_->raise<|System.NotImplementedException()