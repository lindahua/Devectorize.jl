# Unit testing for texpr

using Test
using DeExpr

###########################################################
#
#	simple expression construction
#
###########################################################

# tnum

ex = tnum(1.5)
@test isa(ex, TNum{Float64})
@test ex.val == 1.5
@test ex == tnum(1.5)

# tvar & tscalarvar

ex = tvar(:a)
@test isa(ex, TVar)
@test ex.name == :a
@test ex == tvar(:a)

ex = tscalarvar(:a)
@test isa(ex, TScalarVar)
@test ex.name == :a
@test ex == tscalarvar(:a)

# tqvar

ex = tqvar(:(a.b))
@test isa(ex, TQVar)
@test ex.form == :(a.b)
@test ex == tqvar(:(a.b))

ex = tqvar(:(a.b.c))
@test isa(ex, TQVar)
@test ex.form == :(a.b.c)
@test ex == tqvar(:(a.b.c))


###########################################################
#
#	scalar reference expression construction
#
###########################################################

ex = tref(:(a[1]))
@test isa(ex, TRefScalar1)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex == tref(:(a[1]))

ex = tref(:(a[x]))
@test isa(ex, TRefScalar1)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex == tref(:(a[x]))

ex = tref(:(a[1,2]))
@test isa(ex, TRefScalar2)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex.j == 2
@test ex == tref(:(a[1,2]))

ex = tref(:(a[x,2]))
@test isa(ex, TRefScalar2)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex.j == 2
@test ex == tref(:(a[x,2]))

ex = tref(:(a[1,y]))
@test isa(ex, TRefScalar2)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex.j == :y
@test ex == tref(:(a[1,y]))

ex = tref(:(a[x,y]))
@test isa(ex, TRefScalar2)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex.j == :y
@test ex == tref(:(a[x,y]))

ex = tref(:(a.b[2]))
@test isa(ex, TRefScalar1)
@test ex.host == tqvar(:(a.b))
@test ex.i == 2
@test ex == tref(:(a.b[2]))

ex = tref(:(a.b[2,3]))
@test isa(ex, TRefScalar2)
@test ex.host == tqvar(:(a.b))
@test ex.i == 2
@test ex.j == 3
@test ex == tref(:(a.b[2,3]))


###########################################################
#
#	1D array reference expression construction
#
###########################################################

ex = tref(:(a[:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TColon()
@test ex == tref(:(a[:]))

ex = tref(:(a[2:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(2, nothing)
@test ex == tref(:(a[2:]))

ex = tref(:(a[i0:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, nothing)
@test ex == tref(:(a[i0:]))

ex = tref(:(a[3:7]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(3, 7)
@test ex == tref(:(a[3:7]))

ex = tref(:(a[i0:7]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, 7)
@test ex == tref(:(a[i0:7]))

ex = tref(:(a[3:i1]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(3, :i1)
@test ex == tref(:(a[3:i1]))

ex = tref(:(a[i0:i1]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, :i1)
@test ex == tref(:(a[i0:i1]))

ex = tref(:(a.b[:]))
@test isa(ex, TRef1D)
@test ex.host == tqvar(:(a.b))
@test ex.rgn == TColon()
@test ex == tref(:(a.b[:]))


###########################################################
#
#	column reference expression construction
#
###########################################################

ex = tref(:(a[:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.icol == 5
@test ex == tref(:(a[:, 5]))

ex = tref(:(a[2:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(2, nothing)
@test ex.icol == 5
@test ex == tref(:(a[2:, 5]))

ex = tref(:(a[i0:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.icol == 5
@test ex == tref(:(a[i0:, 5]))

ex = tref(:(a[3:7, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, 7)
@test ex.icol == 5
@test ex == tref(:(a[3:7, 5]))

ex = tref(:(a[i0:7, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, 7)
@test ex.icol == 5
@test ex == tref(:(a[i0:7, 5]))

ex = tref(:(a[3:i1, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, :i1)
@test ex.icol == 5
@test ex == tref(:(a[3:i1, 5]))

ex = tref(:(a[i0:i1, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.icol == 5
@test ex == tref(:(a[i0:i1, 5]))


ex = tref(:(a[:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.icol == :j
@test ex == tref(:(a[:, j]))

ex = tref(:(a[2:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(2, nothing)
@test ex.icol == :j
@test ex == tref(:(a[2:, j]))

ex = tref(:(a[i0:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.icol == :j
@test ex == tref(:(a[i0:, j]))

ex = tref(:(a[3:7, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, 7)
@test ex.icol == :j
@test ex == tref(:(a[3:7, j]))

ex = tref(:(a[i0:7, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, 7)
@test ex.icol == :j
@test ex == tref(:(a[i0:7, j]))

ex = tref(:(a[3:i1, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, :i1)
@test ex.icol == :j
@test ex == tref(:(a[3:i1, j]))

ex = tref(:(a[i0:i1, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.icol == :j
@test ex == tref(:(a[i0:i1, j]))

ex = tref(:(a.b[:,j]))
@test isa(ex, TRefCol)
@test ex.host == tqvar(:(a.b))
@test ex.rrgn == TColon()
@test ex.icol == :j
@test ex == tref(:(a.b[:,j]))


###########################################################
#
#	row reference expression construction
#
###########################################################

ex = tref(:(a[6, :]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TColon()
@test ex == tref(:(a[6, :]))

ex = tref(:(a[6, 2:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[6, 2:]))

ex = tref(:(a[6, i0:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, nothing)
@test ex == tref(:(a[6, i0:]))

ex = tref(:(a[6, 3:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[6, 3:7]))

ex = tref(:(a[6, i0:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, 7)
@test ex == tref(:(a[6, i0:7]))

ex = tref(:(a[6, 3:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(3, :i1)
@test ex == tref(:(a[6, 3:i1]))

ex = tref(:(a[6, i0:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, :i1)
@test ex == tref(:(a[6, i0:i1]))

ex = tref(:(a[i, :]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TColon()
@test ex == tref(:(a[i, :]))

ex = tref(:(a[i, 2:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i, 2:]))

ex = tref(:(a[i, i0:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, nothing)
@test ex == tref(:(a[i, i0:]))

ex = tref(:(a[i, 3:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i, 3:7]))

ex = tref(:(a[i, i0:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, 7)
@test ex == tref(:(a[i, i0:7]))

ex = tref(:(a[i, 3:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(3, :i1)
@test ex == tref(:(a[i, 3:i1]))

ex = tref(:(a[i, i0:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, :i1)
@test ex == tref(:(a[i, i0:i1]))

ex = tref(:(a.b[i, :]))
@test isa(ex, TRefRow)
@test ex.host == tqvar(:(a.b))
@test ex.irow == :i
@test ex.crgn == TColon()
@test ex == tref(:(a.b[i, :]))


###########################################################
#
#	2D reference expression construction
#
###########################################################

ex = tref(:(a[:, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TColon()
@test ex == tref(:(a[:, :]))

ex = tref(:(a[:, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[:, 2:]))

ex = tref(:(a[:, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[:, j0:]))

ex = tref(:(a[:, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[:, 3:7]))

ex = tref(:(a[:, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[:, j0:7]))

ex = tref(:(a[:, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[:, 3:j1]))

ex = tref(:(a[:, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[:, j0:j1]))


ex = tref(:(a[i0:, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TColon()
@test ex == tref(:(a[i0:, :]))

ex = tref(:(a[i0:, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i0:, 2:]))

ex = tref(:(a[i0:, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[i0:, j0:]))

ex = tref(:(a[i0:, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i0:, 3:7]))

ex = tref(:(a[i0:, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[i0:, j0:7]))

ex = tref(:(a[i0:, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[i0:, 3:j1]))

ex = tref(:(a[i0:, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[i0:, j0:j1]))


ex = tref(:(a[i0:i1, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TColon()
@test ex == tref(:(a[i0:i1, :]))

ex = tref(:(a[i0:i1, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i0:i1, 2:]))

ex = tref(:(a[i0:i1, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[i0:i1, j0:]))

ex = tref(:(a[i0:i1, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i0:i1, 3:7]))

ex = tref(:(a[i0:i1, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[i0:i1, j0:7]))

ex = tref(:(a[i0:i1, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[i0:i1, 3:j1]))

ex = tref(:(a[i0:i1, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[i0:i1, j0:j1]))

ex = tref(:(a.b[i0:i1, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tqvar(:(a.b))
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a.b[i0:i1,j0:j1]))


###########################################################
#
#	Fun call expression construction
#
###########################################################

# maps

ex = tcall(:(sin(a)))
@test isa(ex, TMap)
@test ex.fun == :sin
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)
@test ex == tcall(:(sin(a)))

ex = tcall(:(a + b))
@test isa(ex, TMap)
@test ex.fun == :(+)
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)
@test ex == tcall(:(a + b))

ex = tcall(:(clamp(a, b, c)))
@test isa(ex, TMap)
@test ex.fun == :clamp
@test length(ex.args) == 3
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)
@test ex.args[3] == tvar(:c)
@test ex == tcall(:(clamp(a, b, c)))

ex = tcall(:(a + b .* c))
@test isa(ex, TMap)
@test ex.fun == :+
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tcall(:(b .* c))

ex = tcall(:(a.x + max(b[2], c[:,i])))
@test isa(ex, TMap)
@test ex.fun == :+
@test length(ex.args) == 2
@test ex.args[1] == tqvar(:(a.x))
@test ex.args[2].fun == :max
@test ex.args[2].args[1] == tref(:(b[2]))
@test ex.args[2].args[2] == tref(:(c[:,i]))

# reductions

ex = tcall(:(sum(a)))
@test isa(ex, TReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(max(a)))
@test isa(ex, TReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(min(a)))
@test isa(ex, TReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(mean(a)))
@test isa(ex, TReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(dot(a, b)))
@test isa(ex, TReduc)
@test ex.fun == :dot
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)

ex = tcall(:(sum(a, 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(max(a, (), 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(min(a, (), 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(mean(a, 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(sum(a, 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(max(a, (), 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(min(a, (), 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = tcall(:(mean(a, 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)


ex = tcall(:(sum(a[1,2] + b .* c[:,:])))
@test isa(ex, TReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))

ex = tcall(:(sum(a[1,2] + b .* c[:,:], 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))

ex = tcall(:(sum(a[1,2] + b .* c[:,:], 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))

# assignment

ex = tassign(:(a = b))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:a)
@test ex.rhs == tvar(:b)
@test is_trivial_assignment(ex)
@test ex == tassign(:(a = b))

