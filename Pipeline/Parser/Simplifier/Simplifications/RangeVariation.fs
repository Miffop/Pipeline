namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type RangeVariation() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        let current = code.[index]
        if current.Type = "Word" then
            match code[index].Content with
            //Р.п
            |"отрезка"
            //Д.п
            |"отрезку"
            //Т.п
            |"отрезком"
            //П.п
            |"отрезке"
                ->
                Some<|{NewCode = Token("отрезок",code.[index])::[];Length=1;Resimplify=false}
            |_->None 
        else
            None