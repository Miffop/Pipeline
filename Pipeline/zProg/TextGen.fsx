open System.IO

let wr = new StreamWriter("./prog.txt")
wr.WriteLine("применить ВводВывод")
wr.WriteLine("  начало")
for i in 1..500 do
    wr.WriteLine(sprintf "  >>вывод \"Привет%i\"" i)
wr.Close()
