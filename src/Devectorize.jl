module Devectorize


module Internal
    import Base: ==

    # exports
    export 

    # texpr.jl
    TExpr, TNum, TVar, TRef, TMap, TReduc,
    TGenericExpr, TGenericCall, TReducDim, 
    TAssignment, TBlockExpr, TColon,
    texpr, rtype

    # source files
    include("funmeta.jl")
    include("texpr.jl")

end  # module Internal
    
end
