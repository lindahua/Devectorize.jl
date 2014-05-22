# Typed expressions


#################################################
#
#  TExpr and subtypes
#
#################################################

abstract TExpr
isscalar(te::TExpr) = false
asscalar(te::TExpr) = te

# generic expression
immutable TGenericExpr <: TExpr
    intern::Expr
    isscalar::Bool
end
TGenericExpr(x::Expr; isscalar::Bool=false) = TGenericExpr(x, isscalar)
isscalar(te::TGenericExpr) = te.isscalar
asscalar(te::TGenericExpr) = TGenericExpr(te.intern, true)

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
asscalar(te::TVar) = TVar(te.name, true)

# generic function call
immutable TGenericCall <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    isscalar::Bool
end
TGenericCall(f::Symbol, args::Vector{TExpr}; isscalar::Bool=false) = TGenericCall(f, args, isscalar)
isscalar(te::TGenericCall) = te.isscalar
asscalar(te::TGenericCall) = TGenericCall(te.fun, te.args, true)

# a function call for element-wise mapping
immutable TMap <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    isscalar::Bool
end
TMap(f::Symbol, args::Vector{TExpr}; isscalar::Bool=false) = TMap(f, args, isscalar)
isscalar(te::TMap) = te.isscalar
asscalar(te::TMap) = TMap(te.fun, te.args, true)

# a function call for full reduction 
immutable TReduc <: TExpr
    fun::Symbol
    arg::TExpr
end
isscalar(te::TReduc) = true

# a function call for reduction along a certain dimension
immutable TReducDim <: TExpr
    fun::Symbol
    arg::TExpr
    dim::TExpr
end
isscalar(te::TReducDim) = false
asscalar(te::TReducDim) = error("Reduction along dimension cannot be casted as a scalar.")

# reference
immutable TRef <: TExpr
    parent::TExpr
    args::Vector{TExpr}
    isscalar::Bool
end
TRef(parent::TExpr, args::Vector{TExpr}; isscalar::Bool=false) = TRef(parent, args, isscalar)
isscalar(te::TRef) = te.isscalar
asscalar(te::TRef) = TRef(te.parent, te.args, true)

# assignment
immutable TAssignment <: TExpr
    lhs::TExpr
    rhs::TExpr
    isscalar::Bool
end
TAssignment(lhs::TExpr, rhs::TExpr; isscalar::Bool=false) = TAssignment(lhs, rhs, isscalar)
isscalar(te::TAssignment) = te.isscalar
asscalar(te::TAssignment) = TAssignment(te.lhs, te.rhs, true)

# block
immutable TBlockExpr <: TExpr
    exprs::Vector{TExpr}
end
asscalar(te::TBlockExpr) = error("Block expression cannot be casted as a scalar.")

#################################################
#
#   Expression parsing
#
#################################################

texpr(x::Number) = TNum(x)
texpr(x::Symbol) = TVar(x)

function texpr(x::Expr)
    h = x.head
    if h == :call
        f = x.args[1]::Symbol
        if f == :scalar
            @assert length(x.args) == 2
            return asscalar(texpr(x.args[2]))
        else
            targs = TExpr[texpr(a) for a in x.args[2:end]]
            if isewisefun(f)
                if all(a->isa(a, TNum), targs)  # constant propagation
                    return TNum(eval(x))
                else
                    return TMap(f, targs; isscalar=all(isscalar, targs))
                end
            elseif isreducfun(f)
                nargs = length(targs)
                if nargs == 1
                    return TReduc(f, targs[1])
                elseif nargs == 2
                    return TReducDim(f, targs[2], targs[3])
                else
                    error("Devectorize: unsupported reduction with more than two arguments.")
                end
            else
                return TGenericCall(f, targs)
            end
        end

    elseif h == :ref
        p = texpr(x.args[1])
        targs = TExpr[texpr(a) for a in x.args[2:end]]
        return TRef(p, targs)

    elseif h == :(=)
        @assert length(x.args) == 2
        lhs = texpr(x.args[1])
        rhs = texpr(x.args[2])
        return TAssignment(lhs, rhs, isscalar(lhs) || isscalar(rhs))

    elseif h == :block
        return TBlockExpr(TExpr[texpr(a) for a in x.args])

    else
        return TGenericExpr(x)
    end
end



