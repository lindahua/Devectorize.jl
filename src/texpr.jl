# Typed expressions


#################################################
#
#  TExpr and subtypes
#
#################################################

abstract TExpr
isscalar(te::TExpr) = false

# generic expression
immutable TGenericExpr <: TExpr
    intern::Expr
    isscalar::Bool
end
TGenericExpr(x::Expr; isscalar::Bool=false) = TGenericExpr(x, isscalar)
isscalar(te::TGenericExpr) = te.isscalar

# a number literal
immutable TNum <: TExpr
    value::Number
end
isscalar(te::TNum) = true

# a variable (can be either an array or a scalar)
immutable TVar <: TExpr
    name::Symbol
    isscalar::Bool
end
TVar(s::Symbol; isscalar::Bool=false) = TVar(s, isscalar)
isscalar(te::TVar) = te.isscalar

# a function call for element-wise mapping
immutable TMap <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    isscalar::Bool
end
TMap(f::Symbol, args::Vector{TExpr}; isscalar::Bool=false) = TMap(f, args, isscalar)
isscalar(te::TMap) = te.isscalar

# a function call for full reduction 
immutable TReduc <: TExpr
    fun::Symbol
    args::Vector{TExpr}
end
isscalar(te::TReduc) = true

# a function call for reduction along a certain dimension
immutable TReducDim <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    dim::Union(Integer,Symbol)
end
isscalar(te::TReducDim) = false


#################################################
#
#   Expression parsing
#
#################################################



