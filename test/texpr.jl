# test texpr.jl

using Devectorize.Internal
using Base.Test

# TNum
@test texpr(1) == TNum(1)
@test texpr(2.5) == TNum(2.5)
@test rtype(TNum(1.2)) == Float64

# TVar
@test texpr(:abc) == TVar(:abc)
@test texpr(:(x::Float64)) == TVar(:x, Float64)
@test rtype(TVar(:a)) == Any

# function calls
@test texpr(:(foo(x))) == TGenericCall(:foo, TExpr[TVar(:x)])  
@test texpr(:(foo(x::Real))) == TGenericCall(:foo, TExpr[TVar(:x, Real)], Any)
@test texpr(:(foo(x)::Real)) == TGenericCall(:foo, TExpr[TVar(:x, Any)], Real)

@test texpr(:(abs(x))) == TMap(:abs, TExpr[TVar(:x)])
@test texpr(:(x + y)) == TMap(:+, TExpr[TVar(:x), TVar(:y)])

@test texpr(:(abs2(x::Real))) == TMap(:abs2, TExpr[TVar(:x, Real)], Number)
@test texpr(:(x::Array + y::Real)) == TMap(:+, TExpr[TVar(:x, Array), TVar(:y, Real)], Array)
@test texpr(:(x::Int + 3)) == TMap(:+, TExpr[TVar(:x, Int), TNum(3)], Number)

@test texpr(:(abs2(5))) == TNum(25)
@test texpr(:(3 + 4)) == TNum(7)

@test texpr(:(sum(x))) == TReduc(:sum, TVar(:x))
@test rtype(TReduc(:sum, TVar(:x))) == Number

@test texpr(:(sum(4.5))) == TNum(4.5)

@test texpr(:(prod(x, 2))) == TReducDim(:prod, TVar(:x), TNum(2))

# TColon
@test texpr(:(:)) == TColon(:(:), Colon)
@test texpr(:(i:j)) == TColon(:(i:j), Range)
@test texpr(:(i:j:k)) == TColon(:(i:j:k), Range)

# TRef
@test texpr(:(a[])) == TRef(TVar(:a), TExpr[])
@test texpr(:(a[i])) == TRef(TVar(:a), TExpr[TVar(:i)])
@test texpr(:(a[i]::Real)) == TRef(TVar(:a), TExpr[TVar(:i)], Real)
@test texpr(:(a[i:end, :])) == TRef(TVar(:a), TExpr[TColon(:(i:end)), TColon()])

# TAssignment
@test texpr(:(a = b)) == TAssignment(TVar(:a), TVar(:b))
@test texpr(:(a[i:j] = sin(x))) == TAssignment(texpr(:(a[i:j])), texpr(:(sin(x))))

# TBlock
@test texpr(:(a = 1; b[1]; sin(x))) == TBlockExpr(TExpr[texpr(:(a = 1)), texpr(:(b[1])), texpr(:(sin(x)))])
