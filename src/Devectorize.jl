module Devectorize


module Internal
    import Base: ==

    # exports
    export 

    # texpr.jl
    TExpr, TNum, TVar, TRef, TMap, TReduc,
    TGenericExpr, TGenericCall, TReducDim, 
    TAssignment, TBlockExpr, 
    texpr, isscalar, asscalar


    # source files
    include("funmeta.jl")
    include("texpr.jl")

end  # module Internal
    
end
