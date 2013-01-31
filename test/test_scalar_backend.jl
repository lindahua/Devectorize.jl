# Test scalar-backend

#require("../src/de_eval_base.jl")
#require("../src/scalar_backend.jl")

using DeExpr
using Test

a = [1., 2., 3., 4.]
b = [3., 4., 5., 6.]
c = [9., 8., 7., 6.]
cv = 12.0

println("testing unary expression ...")
@devec r1 = -a
@test isequal(r1, -a)

println("testing binary expression ...")
@devec r2 = a .* b
@test isequal(r2, a .* b)
r2_0 = r2
@devec r2[:] = a + b
@test r2 === r2_0
@test isequal(r2, a + b)

println("testing ternary expression ...")
@devec r3 = +(a, b, c)
@test isequal(r3, a + b + c)
r3_0 = r3
@devec r3[:] = +(a, b, c)
@test r3 === r3_0

println("testing compound expression ...")
@devec r4 = (a .* a + b .* b) - (a .* b .* c)
rr4 = (a .* a + b .* b) - (a .* b .* c)
@test isequal(r4, rr4)

println("testing sum of unary expression ...")
@devec s = sum(b)
@test s == sum(b)

println("testing sum of binary expression ...")

@devec s = sum(b .* c)
@test s == dot(b, c)

@devec s = sum(b .* cv)
@test s == cv * sum(b)

@devec s = sum(cv .* b)
@test s == cv * sum(b)

println("testing mean, max, min ...")

@devec s = mean(b)
@test s == mean(b)

@devec s = max(b)
@test s == max(b)

@devec s = max(c)
@test s == max(c)

@devec s = min(b)
@test s == min(b)

@devec s = min(c)
@test s == min(c)




