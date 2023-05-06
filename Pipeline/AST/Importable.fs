namespace Pipeline.AST

type PipelineImportable = 
    abstract Import:Expression
type PipelineNamedImportable = 
    inherit PipelineImportable
    abstract Name:string

