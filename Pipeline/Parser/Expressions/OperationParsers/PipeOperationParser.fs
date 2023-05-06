namespace Pipeline.Parser.Expressions.OperationParsers

open Pipeline.AST
open Pipeline.Parser.Tokens
open Pipeline.Parser.Expressions

type PipeOperationParser() = 
    inherit SeparatorOperationParser()
    override this.GetPriority(op) = 
        match op.Content with
        |"|>"|"|!>"(*|">>="*)|">>"
            ->20
        |_->(-1)
    override this.Nullari(op,strImage) = 
        match op.Content with
        |"|>"->F Function.I
        //|">>="->BindFuncLiteralExpression(Some strImage)
        |">>"->Apply(F Function.C,F Function.B)
        |_->raise<|System.NotImplementedException()
    override this.UnaryLeft(op,l,strImage) =
        match op.Content with
        |"|>"->raise<|OperationIsNotUnary(op.Content)
        |"|!>"->l
        |_->raise<|System.NotImplementedException()
    override this.UnaryRight(op,r,strImage) = 
        this.UnaryLeft(op,r,strImage)
    override this.Binary(op,l,r,strImage) = 
        match op.Content with
        |"|>"
        |"|!>"->Apply(r,l)
        //|">>="->BindExpression(l,r,Some strImage)
        |">>"->Apply(Apply(Apply(F Function.C,F Function.B),l),r)
        |_->raise<|System.NotImplementedException()