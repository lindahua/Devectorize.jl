# Typed expressions

abstract TExpr

# generic expression
immutable TGenericExpr <: TExpr
    intern::Expr
    isscalar::Bool
end

# a number literal
immutable TNum <: TExpr
    value::Number
end

# a variable (can be either an array or a scalar)
immutable TVar <: TExpr
    name::Symbol
    isscalar::Bool
end

# a function call for element-wise mapping
immutable TMap <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    isscalar::Bool
end

# a function call for full reduction 
immutable TReduc <: TExpr
    fun::Symbol
    args::Vector{TExpr}
end

# a function call for reduction along dims
immutable TReduc <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    dim::Union(Integer,Symbol)
end

