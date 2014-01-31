# Test special handling of scalars

import Devectorize
import Devectorize.@devec
import Devectorize.@inspect_devec
import Devectorize.dump_devec
import Devectorize.sqr

using Base.Test

a = [1., 2., 3., 4., 5., 6., 7., 8.]

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

@devec v = -a[3]
@test isequal(v, -a[3])

@devec v = 2. + a[3]
@test isequal(v, 2. + a[3])


r[1] = 0.
@devec r[1] = scalar(v)
@test isequal(r, [v])

i = 2
# @devec v = scalar(a[i]) # TODO: make this work
@devec v = a[i]
@test isequal(v, a[i])

@devec v = -scalar(a[3])
@test isequal(v, -a[3])

@devec v = 2. + scalar(a[3])
@test isequal(v, 2. + a[3])
