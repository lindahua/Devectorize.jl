# Unit testing for element-wise expressions on scalar backend

import Devectorize
import Devectorize.@devec
import Devectorize.@inspect_devec
import Devectorize.dump_devec
import Devectorize.sqr
import Devectorize.blend

using Base.Test

a = [1., 2., 3., 4., 5., 6., 7., 8.]
b = [3., 4., 5., 6., 8., 7., 6., 5.]
c = [9., 8., 7., 6., 4., 2., 3., 1.]
cv = 12.0
cv_a = [12., 12., 12., 12., 12., 12., 12., 12.]

abc = [a b c]

#################################################
#
#   basic element-wise maps
#
#################################################

# unary

@devec r = -a
@test isequal(r, -a)

r[:] = 0.
@devec r[:] = -a
@test isequal(r, -a)

# binary

@devec r = a + b
@test isequal(r, a + b)

r[:] = 0.
@devec r[:] = a + b
@test isequal(r, a + b)

@devec r = a + 1.
@test isequal(r, a .+ 1.)

r[:] = 0.
@devec r[:] = a + 1.
@test isequal(r, a .+ 1.)

@devec r = 1. + b
@test isequal(r, 1. .+ b)

r[:] = 0.
@devec r[:] = 1. + b
@test isequal(r, 1. .+ b)

@devec r[:] = 1. + 2.
@test isequal(r, fill(3., size(a)))

# ternary

@devec r = +(a, b, c)
@test isequal(r, a + b + c)

r[:] = 0.
@devec r[:] = +(a, b, c)
@test isequal(r, a + b + c)

@devec r = +(1., b, c)
@test isequal(r, 1. .+ b .+ c)

r[:] = 0.
@devec r[:] = +(1., b, c)
@test isequal(r, 1. .+ b .+ c)

@devec r = +(a, 2., c)
@test isequal(r, a .+ 2. .+ c)

r[:] = 0.
@devec r[:] = +(a, 2., c)
@test isequal(r, a .+ 2. .+ c)

@devec r = +(a, b, 3.)
@test isequal(r, a .+ b .+ 3.)

r[:] = 0.
@devec r[:] = +(a, b, 3.)
@test isequal(r, a .+ b .+ 3.)

@devec r = +(1., 2., c)
@test isequal(r, 1. .+ 2. .+ c)

r[:] = 0.
@devec r[:] = +(1., 2., c)
@test isequal(r, 1. .+ 2. .+ c)

@devec r = +(1., b, 3.)
@test isequal(r, 1. .+ b .+ 3.)

r[:] = 0.
@devec r[:] = +(1., b, 3.)
@test isequal(r, 1. .+ b .+ 3.)

@devec r = +(a, 2., 3.)
@test isequal(r, a .+ 2. .+ 3.)

r[:] = 0.
@devec r[:] = +(a, 2., 3.)
@test isequal(r, a .+ 2. .+ 3.)

@devec r[:] = +(1., 2., 3.)
@test isequal(r, fill(6., size(a)))


# comparison and blend

@devec r = blend(a .+ 2 .>= b, a .+ 2, b)
@test isequal(r, max(a .+ 2, b))

r[:] = 0.
@devec r[:] = blend(a .+ 2 .>= b, a .+ 2, b)
@test isequal(r, max(a .+ 2, b))

@devec r = blend(a .+ 2 .<= b, a .+ 2, b)
@test isequal(r, min(a .+ 2, b))

r[:] = 0.
@devec r[:] = blend(a .+ 2 .<= b, a .+ 2, b)
@test isequal(r, min(a .+ 2, b))


#################################################
#
#   interaction with qualified names
#
#################################################

type S
    a::Array
    b::Array
    c::Array
end

s = S(a, b, c)

@devec r = sqr(s.a)
@test isequal(r, sqr(s.a))

@devec r = s.a + s.b .* s.c
@test isequal(r, (s.a + s.b .* s.c))

@devec r = +(s.a, s.b, s.c)
@test isequal(r, s.a + s.b + s.c)


#################################################
#
#   interaction with references
#
#################################################

@devec r = sqr(abc[:,1])
@test isequal(r, sqr(a))

@devec r = sqr(abc[:,:])
@test isequal(r, sqr(abc))

@devec r = abc[:,:] .* cv
@test isequal(r, abc .* cv)

j = 3
@devec r = abc[:,1] + b[:] .* abc[:,j]
@test isequal(r, a .+ b .* c)

r = zeros(8, 3)
rr = zeros(size(r))
@devec r[:,2:3] = abc[:, 1:2] .+ cv
rr[:,2:3] = abc[:, 1:2] .+ cv
@test isequal(r, rr)


#################################################
#
#   op-assignment
#
#################################################

r = a
@devec r += b
@test isequal(r, a + b)

r0 = r
@devec r[:] += c
@test r === r0
@test isequal(r, a + b + c)

r = a
@devec r .*= 2.
@test isequal(r, a .* 2)

@devec r -= (c + 3)
@test isequal(r, a .* 2 - (c .+ 3))


#################################################
#
#   blocks
#
#################################################

@devec begin
    x = a + b .* c
    y = sqr(a) + c
end
@test isequal(x, a + b .* c)
@test isequal(y, sqr(a) + c)

