# Typed expressions

#################################################
#
#  TExpr and subtypes
#
#################################################

abstract TExpr
rtype(te::TExpr) = te.rtype

function restrict_type(t0::Type, t1::Type) 
    t = typeintersect(t0, t1)
    t != None || error("Devectorize: type conflict.")
    t
end

# generic expression
immutable TGenericExpr <: TExpr
    intern::Expr
    rtype::Type
end
TGenericExpr(x::Expr) = TGenericExpr(x, Any)
cast(te::TGenericExpr, t::Type) = TGenericExpr(te.intern, restrict_type(te.rtype, t))

# a number literal
immutable TNum <: TExpr
    value::Number
end
rtype(te::TNum) = typeof(te.value)
cast(te::TNum, t::Type) = (isa(value, t) || error("Devectorize: type conflict."))

# a variable (can be either an array or a scalar)
immutable TVar <: TExpr
    name::Symbol
    rtype::Type
end
TVar(s::Symbol) = TVar(s, Any)
cast(te::TVar, t::Type) = TVar(te.name, restrict_type(te.rtype, t))

# generic function call
immutable TGenericCall <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    rtype::Type
end
TGenericCall(f::Symbol, args::Vector{TExpr}) = TGenericCall(f, args, Any)
cast(te::TGenericCall, t::Type) = TGenericCall(te.fun, te.args, restrict_type(te.rtype, t))
== (x::TGenericCall, y::TGenericCall) = (x.fun == y.fun && x.args == y.args && x.rtype == y.rtype)

# a function call for element-wise mapping
immutable TMap <: TExpr
    fun::Symbol
    args::Vector{TExpr}
    rtype::Type
end
TMap(f::Symbol, args::Vector{TExpr}) = TMap(f, args, Any)
cast(te::TMap, t::Type) = TMap(te.fun, te.args, restrict_type(te.rtype, t))
== (x::TMap, y::TMap) = (x.fun == y.fun && x.args == y.args && x.rtype == y.rtype)

# a function call for full reduction 
immutable TReduc <: TExpr
    fun::Symbol
    arg::TExpr
    rtype::Type
end
TReduc(f::Symbol, arg::TExpr) = TReduc(f, arg, Number)
cast(te::TReduc, t::Type) = TReduc(te.fun, te.arg, restrict_type(te.rtype, t))

# a function call for reduction along a certain dimension
immutable TReducDim <: TExpr
    fun::Symbol
    arg::TExpr
    dim::TExpr
    rtype::Type
end
TReducDim(f::Symbol, arg::TExpr, dim::TExpr) = TReducDim(f, arg, dim, AbstractArray)
cast(te::TReducDim, t::Type) = TReducDim(te.fun, te.arg, te.dim, restrict_type(te.rtype, t))

# reference

immutable TColon <: TExpr
    args::Vector{TExpr}
    rtype::Type
end
TColon() = TColon(TExpr[], Colon)
TColon(args::Vector{TExpr}) = TColon(args, Range)
cast(te::TColon, t::Type) = TColon(te.args, restrict_type(te.rtype, t))
== (x::TColon, y::TColon) = (x.args == y.args && x.rtype == y.rtype)

immutable TRef <: TExpr
    parent::TExpr
    args::Vector{TExpr}
    rtype::Type
end
TRef(parent::TExpr, args::Vector{TExpr}) = TRef(parent, args, Any)
cast(te::TRef, t::Type) = TRef(te.parent, te.args, restrict_type(te.rtype, t))
== (x::TRef, y::TRef) = (x.parent == y.parent && x.args == y.args && x.rtype == y.rtype)

# assignment
immutable TAssignment <: TExpr
    lhs::TExpr
    rhs::TExpr
end
rtype(te::TAssignment) = rtype(te.rhs)

# block
immutable TBlockExpr <: TExpr
    exprs::Vector{TExpr}
end
rtype(te::TBlockExpr) = error("Taking rtype of a block expression is not supported.")


#################################################
#
#   Expression parsing
#
#################################################

texpr(x::Number) = TNum(x)
texpr(x::Symbol) = TVar(x)

function maprtype(rtypes::Vector{Type})
    c = 0
    for t in rtypes
        if t <: Number
            c = max(c, 0)
        elseif t <: Array
            c = max(c, 1)
        elseif t <: DenseArray
            c = max(c, 2)
        elseif t <: AbstractArray
            c = max(c, 3)
        else
            c = max(c, 100)
        end
    end
    c == 0 ? Number : 
    c == 1 ? Array :
    c == 2 ? DenseArray :
    c == 3 ? AbstractArray : Any
end

function texpr(x::Expr)
    h = x.head
    if h == :(::)
        @assert length(x.args) == 2
        t = eval(x.args[2])
        isa(t, Type) || error("Devectorize: The second argument to :: should be a type name.")
        return cast(texpr(x.args[1]), t)

    elseif h == :call
        f = x.args[1]::Symbol
        targs = TExpr[texpr(a) for a in x.args[2:end]]
        if isewisefun(f)
            if all(a->isa(a, TNum), targs)  # constant propagation
                return TNum(eval(x))
            else
                return TMap(f, targs, maprtype(Type[rtype(a) for a in targs]))
            end
        elseif isreducfun(f)
            nargs = length(targs)
            if nargs == 1
                a = targs[1]
                if isa(a, TNum)     # constant propagation
                    return TNum(eval(x))
                else
                    return TReduc(f, targs[1])
                end
            elseif nargs == 2
                return TReducDim(f, targs[1], targs[2])
            else
                error("Devectorize: unsupported reduction with more than two arguments.")
            end
        else
            return TGenericCall(f, targs)
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
