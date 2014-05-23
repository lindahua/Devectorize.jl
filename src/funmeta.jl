# meta information about functions

const ewisefuns = Set([
    :+, :-, :*, :/, :^, 
    :.+, :.-, :.*, :./, :.^, 
    :abs, :abs2, :sqrt, :cbrt, 
    :exp, :log, 
    :sin, :cos, :tan])

const reducfuns = Set([
    :sum, :maximum, :minimum, :mean, :prod, 
    :all, :any])


isewisefun(f::Symbol) = (f in ewisefuns)
isreducfun(f::Symbol) = (f in reducfuns)

