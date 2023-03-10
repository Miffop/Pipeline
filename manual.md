# "Пусть"

в языке нет переменных все данные константны.
чтобы создать констануту, воспользуйтесь ключевым словом **"пусть"** после напишите название константы и затем её значение.
например:
```
# однострочные коментарии обозначаются решёткой
пусть икс 5
пусть дробь 0.5
пусть строка "Привет мир!"
```
названия констант должны содержать в себе только буквы и цифры. название константы всегда должно начинаться с буквы.
чтобы избавиться от этих ограничений, нужно заключить название в обратные апострофы:
```
пусть `2 + 2` 4
пусть `два слова` "как дела?"
```

# Арифметика

при определении констант в них можно записывать не только литералы, но и выражения содержашие другие константы и различные операции
(сложение - "+", умножение - "\*", вычитание - "-", деление - "/").
```
пусть а 2+2*2 # а = 6
пусть б а*3-4 # б = 14
```
в выражениях можно испольовать скобки для изменения приоритетов операций
```
пусть а (2+2)*2 # а = 8
пусть б а*(3-4) # б = -8
```

# Функции

данный язык предоставляет возможность создания анонимных функций с одним аргументом при помоши ключегого слова **"от"**, после котого следует имя зависимой переменной
и затем тело функции.
```
от икс икс*икс
```
функцию можно записать в константу.
```
пусть квадрат от икс икс*икс
```
чтобы подставить значение в функцию, нужно просто записать его после определения функции
```
пусть а (от а а*2) 4 # а = 8
пусть б квадрат 18 # б = 324
``` 
функции от нескольких переменных определяются с помощью каррирования тоесть функций высшых порядков
```
# скобки в обеих строчках необязательны, они показывают порядок выполения операций
пусть числа (от а (от б а+б)) # создание функции сложения от 2 аргументов
пусть результат ((числа 2) 3) # применение функции к числам 2 и 3
```
любую операцую в языке можно превратить функцию заключив её в скобки, но порядок аргументов зачастю меняется, тоесть `2 - 3` становиться `(-)3 2`
```
пусть а (+)2 3 # 5
пусть б (-)4 a # 1
```
каррирование также позволяет использовать частичное применение функции
```
пусть а (+)3 # a = от икс икс + 3
пусть б а 2 # б = 5
```

# Конвеер

Перенос на новую строку является значимым в этом языке, значение на предыдущей сточке применяется к функции на следующей
```
2
(+)3
# вернёт 5
```
```
2
(от а а*2) # 4
(-)3 # 1
(от а от б (а+б)*а) 3
# вернёт 12
```
перенос не является значашим если строка заканчивается на оператор
```
2+
2*
2
# вернёт 6
```

# Отступы

отступы в языке выделяют блоки кода, например: многострочные функции
```
пусть п2у2 от а
  a
  (*)2
  (+)2
п2y2 3
# вернёт 8
```
для отступов можно использовать только пробелы.

# Условия

для создания условий используется конструкция **"если _а_ то/вернуть _б_ иначе _ц_"**, 
где _с_ - условие, _б_ - вычисляется если условие верно и _ц_ - вычисляется если условие ложно

```
если а>2 вернуть 
  4
иначе
  2
```
несколько условий можно объединять в цепочку
```
если а > 2 вернуть
  1
иначе если а = 2 то
  0
иначе
  -1
```
условия это выражения поэтому, они могут быть однострочными
```
1 + если а>2 то 1 иначе -1
```

# Базовые типы данных

в языке существуют несколько базовых типов данных:
- **число** - 32 битное целое число со знаком
- **дробь** - 64 битное число с плавающей точкой
- **строка** - набор символов
- **логический тип** - имеет два значения "правда" или "ложь"

все операции могут производится только между двумя значениями одинаковых типов - язык с сильной динамической типизацией, поэтому нет неявного приведения типов
```
2 + 3.0 # приведёт к ошибке
```
чтобы избежать такого рода ошибок нужно использовать функции приведения:
- **"целое"** переводит в **число**
- **"дробь"** переводит в **дробь**
- **"строка"** переводит в **строку**
```
дробь 2 + 3.0 # вернёт 5.0
```

# Маркеры

в языке имеется возможность помечать данные заранее определёнными маркерами, которые ведут себя как обёртка для данных.
чтобы определить маркер, нужно использовать ключевое слово **"маркер"**, затем записать его название и количесво записей для данных в нём
```
маркер пара с 2 записями
маркер ячейка с записью
маркер пустота
```
чтобы создать экземляр маркера, используйте название его типа как функцию примимающую значения его записей
```
пусть а пара 1 2
пусть б ячейка 10
пусть ц пустота
```
чтобы получить доступ к записи экземляра маркера, нужно использовать его название как функцию принимающую номер записи(для маркеров с больше чем 1 записью; нумерация с нуля) и маркер.
```
пусть а1 пара 0 а # а1 = 1
пусть б1 ячейка б # б1 = 10
```
чтобы определить принадлежит ли данный экзмляр определённому маркеру используйте оператор **"?"**
```
пусть а2 а ? пара # а2 = правда
пусть б2 б ? пара # б2 = ложь
пусть ц2 ц ? пара # ц2 = ложь
```

в языке есть несколько встоенных маркеров
- **"есть"**(с записью) и **"нет"**(пустой)
- **"пара"**(с 2 записями)

# Списки

для того чтобы работать со множеством значений одновременно, в языке существуют списки.
есть несколько способов создать список:
- через оператор **","** пречислением элементов: ``1,2,3,[]``
- функцией **"создать"**, которая принимает число элементов списка и функцию от номера элемента возвращающую его значение: ``создать 3 (от ч ч)``

для доступа к элементам списка сущесвуют функции **"голова"** и **"хвост"**
**"голова"** возврашает первый элемент списка, а **"хвост"** - оставшуюся часть
```
пусть сп 1,3,2,[]
пусть г голова сп # г = 1
пусть х хвост сп # х = 3,2,[]
```
опреатор **","** позволяет добавить элемент в начало списка
```
# скробки необязательны и нужны чтобы показать порядок операций
пусть а 1,(3,(2,[]))
пусть б 4,а # б = 4,1,3,2,[] 
```
пустой список обозначается **"[]"**

# Циклы

циклы в данном языке предоставляют ещё одну возможность создавать новые последовательности.
нужно использовать конствукцию **"для _а_ в _б_ вернуть _ц_"** для создания цикл
где _а_ - элемент из _б_, _б_ - последовательность, _ц_ - выражение
```
для ч в 1,3,2,[] вернуть
  ч*ч
# вернёт 1,9,4,[]
```

