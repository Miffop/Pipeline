namespace Pipeline.Parser.Simplifier.Simplifications

open Pipeline.Parser.Tokens
open Pipeline.Parser.Simplifier

type ConvertionVariation() = 
    inherit ISimplification()
    override this.TrySimplify(code,index,length,sp) = 
        let current = code.[index]
        if current.Type = "Keyword" && (current.Content = "к" || current.Content = "как" || current.Content = "в") && index+1<code.Length && code.[index+1].Type = "Word" then
            match code[index+1].Content with
            //целое
            |"целое" when current.Content ="в" || current.Content = "как" -> 
                Some {NewCode = Token("Word","целое",current)::[];Length = 2;Resimplify = true}
            |"целому" when current.Content ="к" -> 
                Some {NewCode = Token("Word","целое",current)::[];Length = 2;Resimplify = true}
            //дробь
            |"дробь" when current.Content ="в" || current.Content = "как" -> 
                Some {NewCode = Token("Word","дробь",current)::[];Length = 2;Resimplify = true}
            |"дроби" when current.Content ="к" -> 
                Some {NewCode = Token("Word","дробь",current)::[];Length = 2;Resimplify = true}
            //строка
            |"строку" when current.Content ="в" || current.Content = "как" -> 
                Some {NewCode = Token("Word","строка",current)::[];Length = 2;Resimplify = true}
            |"строке" when current.Content ="к" -> 
                Some {NewCode = Token("Word","строка",current)::[];Length = 2;Resimplify = true}

            |_->None 
        else
            None