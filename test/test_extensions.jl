
using DeExpr
using Test

#################################################
#
#	devec_transform
#
#################################################

n = 4
d = {:x => rand(n),
     :y => pi * [1:n],
     :z => 1 / [1:n]}
     
@devec_transform d  xd = x .* y + z

@test isequal(d[:xd], d[:x] .* d[:y] + d[:z])

@devec_transform(d,
                 xd => x .* y + z,
                 yd => x .* x)

@test isequal(d[:yd], d[:x] .* d[:x])
