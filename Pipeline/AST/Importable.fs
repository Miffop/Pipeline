namespace Pipeline.AST

type PipelineImportable = 
    abstract Import:PFunOrData
type PipelineNamedImportable = 
    inherit PipelineImportable
    abstract Name:string


open System.Reflection
type PipelineReflectionImporter(importName:string) = 
    inherit IExpression()
    static member ImportAsm(asm:Assembly) = 
        let c = PContext()
        seq{
            for t in asm.GetTypes() do
                let i = t.FindInterfaces((fun x c->x=typedefof<PipelineImportable>),null)
                if i.Length <> 0 && not t.IsInterface && not t.IsAbstract then 
                    yield t
        }
        |>Seq.map(fun t->System.Activator.CreateInstance(t):?>PipelineImportable)
        |>Seq.map(fun t->t,match t with | :? PipelineNamedImportable as t -> t.Name |_->t.GetType().Name)
        |>Seq.iter(fun (t,name)->c.Def name t.Import)
        c
    static member ImportAsm(asmPath:string) = 
        PipelineReflectionImporter.ImportAsm(Assembly.LoadFile(asmPath))
    override this.Eval(c) = 
        match importName with
        |x when x.EndsWith(".dll") -> PipelineReflectionImporter.ImportAsm(x)
        |x when x.EndsWith(".exe") -> PipelineReflectionImporter.ImportAsm(x)
        |_->raise <| System.NotImplementedException()
        |>c.Merge
        Func(Identity())
