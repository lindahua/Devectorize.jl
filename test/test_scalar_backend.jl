# Test scalar-backend

import DeExpr
import DeExpr.@devec
import DeExpr.@inspect_devec
import DeExpr.@fast_reduc
import DeExpr.@inspect_fast_reduc
import DeExpr.sqr
import DeExpr.blend
using Test

# tools to help testing

function dump_devec(ex::Expr)
	println(DeExpr.compile(DeExpr.ScalarContext(), ex))
end


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


#################################################
#
#	simple expressions
#
#################################################

#dump_devec(:(r = a))
@devec r = a
@test isequal(r, a)

r0 = r
@devec r[:] = b
@test r === r0
@test isequal(r, b)

@devec r[:] = 12.
@test isequal(r, cv_a)

@devec r[:] = cv
@test isequal(r, cv_a)


#################################################
#
#	reference expressions
#
#################################################

# 0D

r = similar(a)
@devec r[:] = a[3]
@test isequal(r, fill(a[3], size(a)))

i = 2
@devec r[:] = a[i]
@test isequal(r, fill(a[i], size(a)))

@devec r[:] = abc[1,3]
@test isequal(r, fill(c[1], size(a)))

i = 2
@devec r[:] = abc[i,3]
@test isequal(r, fill(c[i], size(a)))

@devec r[:] = abc[2,i]
@test isequal(r, fill(b[2], size(a)))

j = 1
@devec r[:] = abc[i, j]
@test isequal(r, fill(a[i], size(a)))


# 1D

@devec r = a[:]
@test isequal(r, a)

@devec r[:] = b[:]
@test isequal(r, b)

@devec r[:] = abc[:,1]
@test isequal(r, a)

i = 2
@devec r[:] = abc[:,i]
@test isequal(r, b)

@devec r = abct[1, :]
@test isequal(r, at)

i = 2
@devec r[:] = abct[i, :]
@test isequal(r, bt)

# 2D

@devec r = abc[:,:]
@test isequal(r, abc)

r0 = r
@devec r[:,:] = 1.
@test isequal(r, fill(1., size(abc)))
@test r === r0

@devec r[:,:] = cv
@test isequal(r, fill(cv, size(abc)))

@devec r[:,:] = abc
@test isequal(r, abc)
@test r === r0

i = 2
j = 3
@devec r[:,:] = abc[i,j]
@test isequal(r, fill(c[2], size(abc)))


#################################################
#
#	element-wise maps
#
#################################################

# unary

@devec r = -a
@test isequal(r, -a)

# binary

@devec r = a + b
@test isequal(r, a + b)

@devec r = a + 1.
@test isequal(r, a + 1.)

@devec r = 1. + b
@test isequal(r, 1. + b)

@devec r[:] = 1. + 2.
@test isequal(r, fill(3., size(a)))

# ternary

@devec r = +(a, b, c)
@test isequal(r, a + b + c)

@devec r = +(1., b, c)
@test isequal(r, 1. + b + c)

@devec r = +(a, 2., c)
@test isequal(r, a + 2. + c)

@devec r = +(a, b, 3.)
@test isequal(r, a + b + 3.)

@devec r = +(1., 2., c)
@test isequal(r, 1. + 2. + c)

@devec r = +(1., b, 3.)
@test isequal(r, 1. + b + 3.)

@devec r = +(a, 2., 3.)
@test isequal(r, a + 2. + 3.)

@devec r[:] = +(1., 2., 3.)
@test isequal(r, fill(6., size(a)))


# comparison and blend

@devec r = blend(a + 2 .>= b, a + 2, b)
@test isequal(r, max(a+2, b))

@devec r = blend(a + 2 .<= b, a + 2, b)
@test isequal(r, min(a + 2, b)) 


#################################################
#
#	special handling of scalars
#
#################################################

r = zeros(1)

@devec v = 1.
@test isequal(v, 1.)

@devec r[1] = 2.
@test isequal(r, [2.])

@devec v = r[1]
@test isequal(v, 2.)

r[1] = 0.
@devec r[1] = v
@test isequal(r, [v])

r[1] = 0.
@devec r[1,1] = v
@test isequal(r, [v])

i = 2
@devec v = a[i]
@test isequal(v, a[2])

i = 2
j = 3
@devec v = abc[i, j]
@test isequal(v, c[2])

@devec v = -a[3]
@test isequal(v, -a[3])

@devec v = 2. + a[3]
@test isequal(v, 2. + a[3])


#################################################
#
#	full reduction
#
#################################################

r = zeros(1)
@devec r[1] = sum(a)
@test isequal(r, [sum(a)])

@devec r = sum(a)
@test isequal(r, sum(a))

@devec r = sum(a[:,:])
@test isequal(r, sum(a))

@devec r = sum(abc[:,:])
@test isequal(r, sum(abc))

@devec r = max(a)
@test isequal(r, max(a))

@devec r = max(c)
@test isequal(r, max(c))

@devec r = min(a)
@test isequal(r, min(a))

@devec r = min(c)
@test isequal(r, min(c))

@devec r = mean(a)
@test isequal(r, mean(a))

@devec r = mean(abc[:,:])
@test isequal(r, mean(abc))

@devec r = dot(a, b)
@test isequal(r, dot(a, b))

@devec r = dot(a[:,:], b[:,:])
@test isequal(r, dot(a, b))

@devec r = dot(abc[:,:], abc)
@test isequal(r, dot(abc[:], abc[:]))

#################################################
#
#	partial reduction
#
#################################################

@devec r = sum(abc, 1)
@test isequal(r, sum(abc, 1))

@devec r = sum(abc, 2)
@test isequal(r, sum(abc, 2))

r = zeros(size(abc, 2))
r0 = r
@devec r[:] = sum(abc, 1)
@test r === r0
@test isequal(r, vec(sum(abc, 1)))

r = zeros(size(abc, 1))
r0 = r
@devec r[:] = sum(abc, 2)
@test r === r0
@test isequal(r, vec(sum(abc, 2)))

@devec r = mean(abc, 1)
@test isequal(r, mean(abc, 1))

@devec r = mean(abc, 2)
@test isequal(r, mean(abc, 2))

@devec r = max(abc, 1)
@test isequal(r, max(abc, 1))

@devec r = max(abc, 2)
@test isequal(r, max(abc, 2))

@devec r = min(abc, 1)
@test isequal(r, min(abc, 1))

@devec r = min(abc, 2)
@test isequal(r, min(abc, 2))

@devec r = sum(sqr(abc), 1)
@test isequal(r, sum(abc .* abc, 1))

@devec r = sum(sqr(abc), 2)
@test isequal(r, sum(abc .* abc, 2))


#################################################
#
#	fast reduction
#
#################################################

@fast_reduc Float64[4] r = sum(a)
@test isequal(r, sum(a))

n = 4
@fast_reduc Float64[n] r = sum(a)
@test isequal(r, sum(a))

@fast_reduc Float64[4,3] r = sum(abc)
@test isequal(r, sum(abc))

@fast_reduc Float64[3,n] r = sum(abct)
@test isequal(r, sum(abct))


#################################################
#
#	blocks
#
#################################################

@devec begin
	x = a + b .* c
	s = sum(a)
end
@test isequal(x, a + b .* c)
@test isequal(s, sum(a))


