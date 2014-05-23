# test texpr.jl

using Devectorize.Internal
using Base.Test

# TNum
@test texpr(1) == TNum(1)
@test texpr(2.5) == TNum(2.5)
@test isscalar(TNum(1))

# TVar
@test texpr(:abc) == TVar(:abc)
@test texpr(:(scalar(x))) == TVar(:x, true)

@test !isscalar(TVar(:a))
@test isscalar(TVar(:a, true))

# function calls
@test texpr(:(foo(x))) == TGenericCall(:foo, TExpr[TVar(:x)])  
@test texpr(:(foo(scalar(x)))) == TGenericCall(:foo, TExpr[TVar(:x, true)], false)
@test texpr(:(scalar(foo(x)))) == TGenericCall(:foo, TExpr[TVar(:x, false)], true)

@test texpr(:(abs(x))) == TMap(:abs, TExpr[TVar(:x)])
@test texpr(:(x + y)) == TMap(:+, TExpr[TVar(:x), TVar(:y)])

@test texpr(:(abs2(scalar(x)))) == TMap(:abs2, TExpr[TVar(:x, true)], true)
@test texpr(:(x + scalar(y))) == TMap(:+, TExpr[TVar(:x), TVar(:y, true)], false)
@test texpr(:(scalar(x) + 3)) == TMap(:+, TExpr[TVar(:x, true), TNum(3)], true)

@test texpr(:(abs2(5))) == TNum(25)
@test texpr(:(3 + 4)) == TNum(7)

@test texpr(:(sum(x))) == TReduc(:sum, TVar(:x))
@test isscalar(TReduc(:sum, TVar(:x)))

@test texpr(:(sum(4.5))) == TNum(4.5)

@test texpr(:(prod(x, 2))) == TReducDim(:prod, TVar(:x), TNum(2))

# TRef
@test texpr(:(a[])) == TRef(TVar(:a), TExpr[])

