namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type MarkerSimplification() =
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        let c = 
            code.[index..index+length-1]
            |>List.map(fun x->x.Type,x.Content)
        match c with
        |("Keyword","маркер")::x ->
            match x with
            |("Word",name)::("Keyword","с")::("Int",_)::("Word",sop)::_ when sop = "записью" || sop = "записями"->
                let result = code.[index]::code.[index+1]::code.[index+3]::[]
                Some<|{NewCode = result;Length = 5;Resimplify = false}
            |("Word",name)::("Keyword","с")::("Word","записью")::_->
                let result = code.[index]::code.[index+1]::Token("Int","1",code.[index+2])::[]
                Some<|{NewCode = result;Length = 4;Resimplify = false}
            |("Word",name)::_->
                let result = code.[index]::code.[index+1]::Token("Int","0",code.[index+2])::[]
                Some<|{NewCode = result;Length = 2;Resimplify = false}
            |_->
                failwithf "ошибка в обьявлении маркера"
        |_->None