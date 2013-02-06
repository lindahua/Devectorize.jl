# Test scalar-backend

import DeExpr
import DeExpr.@devec
import DeExpr.@inspect_devec
# import DeExpr.@fast_reduc
# import DeExpr.@inspect_fast_reduc
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
#	full reduction
#
#################################################

# r = zeros(1)
# @devec r[1] = sum(a)
# @test isequal(r, [sum(a)])

# @devec r = sum(a)
# @test isequal(r, sum(a))

# @devec r = sum(a[:,:])
# @test isequal(r, sum(a))

# @devec r = sum(abc[:,:])
# @test isequal(r, sum(abc))

# @devec r = max(a)
# @test isequal(r, max(a))

# @devec r = max(c)
# @test isequal(r, max(c))

# @devec r = min(a)
# @test isequal(r, min(a))

# @devec r = min(c)
# @test isequal(r, min(c))

# @devec r = mean(a)
# @test isequal(r, mean(a))

# @devec r = mean(abc[:,:])
# @test isequal(r, mean(abc))

# @devec r = dot(a, b)
# @test isequal(r, dot(a, b))

# @devec r = dot(a[:,:], b[:,:])
# @test isequal(r, dot(a, b))

# @devec r = dot(abc[:,:], abc)
# @test isequal(r, dot(abc[:], abc[:]))

#################################################
#
#	partial reduction
#
#################################################

# @devec r = sum(abc, 1)
# @test isequal(r, sum(abc, 1))

# @devec r = sum(abc, 2)
# @test isequal(r, sum(abc, 2))

# r = zeros(size(abc, 2))
# r0 = r
# @devec r[:] = sum(abc, 1)
# @test r === r0
# @test isequal(r, vec(sum(abc, 1)))

# r = zeros(size(abc, 1))
# r0 = r
# @devec r[:] = sum(abc, 2)
# @test r === r0
# @test isequal(r, vec(sum(abc, 2)))

# @devec r = mean(abc, 1)
# @test isequal(r, mean(abc, 1))

# @devec r = mean(abc, 2)
# @test isequal(r, mean(abc, 2))

# @devec r = max(abc, 1)
# @test isequal(r, max(abc, 1))

# @devec r = max(abc, 2)
# @test isequal(r, max(abc, 2))

# @devec r = min(abc, 1)
# @test isequal(r, min(abc, 1))

# @devec r = min(abc, 2)
# @test isequal(r, min(abc, 2))

# @devec r = sum(sqr(abc), 1)
# @test isequal(r, sum(abc .* abc, 1))

# @devec r = sum(sqr(abc), 2)
# @test isequal(r, sum(abc .* abc, 2))


#################################################
#
#	fast reduction
#
#################################################

# @fast_reduc Float64[4] r = sum(a)
# @test isequal(r, sum(a))

# n = 4
# @fast_reduc Float64[n] r = sum(a)
# @test isequal(r, sum(a))

# @fast_reduc Float64[4,3] r = sum(abc)
# @test isequal(r, sum(abc))

# @fast_reduc Float64[3,n] r = sum(abct)
# @test isequal(r, sum(abct))


#################################################
#
#	blocks
#
#################################################

# @devec begin
# 	x = a + b .* c
# 	s = sum(a)
# end
# @test isequal(x, a + b .* c)
# @test isequal(s, sum(a))


