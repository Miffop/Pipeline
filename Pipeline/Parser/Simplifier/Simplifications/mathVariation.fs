namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type MathsVariation() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        let current = code.[index]
        if current.Type = "Word" then
            let next = if index+1<code.Length && code.[index+1].Type = "Keyword" then code.[index+1].Content else ""
            match current.Content with
            //сложение
            |"прибавить" when next ="к" -> 
                Some {NewCode = Token("прибавить",current)::[];Length = 2;Resimplify = false}
            |"сложить" when next = "с" ->
                Some {NewCode = Token("прибавить",current)::[];Length = 2;Resimplify = false}           
            |"сложить" -> 
                Some {NewCode = Token("прибавить",current)::[];Length = 1;Resimplify = false}
            |"плюс" -> 
                Some {NewCode = Token("прибавить",current)::[];Length = 1;Resimplify = false}
            |"добавить" ->
                Some {NewCode = Token("прибавить",current)::[];Length = 1;Resimplify = false}
            //вычитание
            |"минус" -> 
                Some {NewCode = Token("вычесть",current)::[];Length = 1;Resimplify = false}
            |"без" ->
                Some {NewCode = Token("вычесть",current)::[];Length = 1;Resimplify = false}
            //умножение
            |"умножить" when next = "на"-> 
                Some {NewCode = Token("умножить",current)::[];Length = 2;Resimplify = false}
            //деление
            |"делить" when next = "на"-> 
                Some {NewCode = Token("разделить",current)::[];Length = 2;Resimplify = false}
            |"разделить" when next = "на"-> 
                Some {NewCode = Token("разделить",current)::[];Length = 2;Resimplify = false}
            //остаток
            |"остаток" when next = "от"-> 
                Some {NewCode = Token("остаток",current)::[];Length = 2;Resimplify = false}
            |_->None

        else
            None