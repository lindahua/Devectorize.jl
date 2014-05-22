# Unit testing for hybrid expressions

using Base.Test
using Devectorize

#data

a = [1., 2., 3., 4.]
b = [3., 4., 5., 6.]
c = [9., 8., 7., 6.]
cv = 12.0
cv_a = [12., 12., 12., 12.]

abc = [a b c]

at = a'
bt = b'
ct = c'
abct = [at, bt, ct]

# test cases

@devec r = sum(a) + 2.
@test isequal(r, sum(a) + 2.)

@devec r = sqrt(sum(sqr(a)))
@test isequal(r, sqrt(sum(a .* a)))

@devec r = sum(abc, 1) + max(c) .* cv
@test isequal(r, sum(abc, 1) .+ maximum(c) .* cv)

@devec r = sum(max(abc, (), 2)) + sum(abct, 1) .* (cv .+ max(b))
@test isequal(r, sum(maximum(abc, 2)) .+ sum(abct, 1) .* (cv .+ maximum(b)) )
