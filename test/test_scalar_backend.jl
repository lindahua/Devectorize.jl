# Test scalar-backend

#require("../src/de_eval_base.jl")
#require("../src/scalar_backend.jl")

using DeExpr
using Test

a = [1., 2., 3., 4.]
b = [3., 4., 5., 6.]
c = [9., 8., 7., 6.]

r1 = zeros(size(a))
r2 = zeros(size(a))
r3 = zeros(size(a))
r4 = zeros(size(a))

@devec r1 = -a
@test isequal(r1, -a)

@devec r2 = a .* b
@test isequal(r2, a .* b)

@devec r3 = +(a, b, c)
@test isequal(r3, a + b + c)

@devec r4 = (a .* a + b .* b) - (a .* b .* c)
rr4 = (a .* a + b .* b) - (a .* b .* c)
@test isequal(r4, rr4)
