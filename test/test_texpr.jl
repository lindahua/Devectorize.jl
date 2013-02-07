# Unit testing for texpr

using Test
using Devectorize

###########################################################
#
#	simple expression construction
#
###########################################################

# tnum

ex = texpr(1.5)
@test isa(ex, TNum{Float64})
@test ex.val == 1.5
@test ex == tnum(1.5)
@test eval(ju_expr(ex)) == 1.5

# tvar & tscalarvar

a = 12.5

ex = texpr(:a)
@test isa(ex, TVar)
@test ex.name == :a
@test ex == tvar(:a)
@test eval(ju_expr(ex)) == a

ex = tscalarvar(:a)
@test isa(ex, TScalarVar)
@test ex.name == :a
@test ex == tscalarvar(:a)
@test eval(ju_expr(ex)) == a

# tqvar

ex = texpr(:(a.b))
@test isa(ex, TQVar)
@test ex.form == :(a.b)
@test ex == tqvar(:(a.b))

type A1
	b
end
a = A1(2.3)
@test eval(ju_expr(ex)) == a.b


ex = texpr(:(a.b.c))
@test isa(ex, TQVar)
@test ex.form == :(a.b.c)
@test ex == tqvar(:(a.b.c))

type A2
	c
end
a = A1(A2(4.5))
@test eval(ju_expr(ex)) == a.b.c


###########################################################
#
#	scalar reference expression construction
#
###########################################################

a = [10 20 30; 40 50 60]

ex = texpr(:(a[1]))
@test isa(ex, TGeneralRef1)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex == tref(:(a[1]))
@test eval(ju_expr(ex)) == a[1] 

ex = texpr(:(a[x]))
@test isa(ex, TGeneralRef1)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex == tref(:(a[x]))
x = 2
@test eval(ju_expr(ex)) == a[x]

ex = texpr(:(a[1,2]))
@test isa(ex, TGeneralRef2)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex.j == 2
@test ex == tref(:(a[1,2]))
@test eval(ju_expr(ex)) == a[1,2]

ex = texpr(:(a[x,2]))
@test isa(ex, TGeneralRef2)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex.j == 2
@test ex == tref(:(a[x,2]))
@test eval(ju_expr(ex)) == a[x,2]

ex = texpr(:(a[1,y]))
@test isa(ex, TGeneralRef2)
@test ex.host == tvar(:a)
@test ex.i == 1
@test ex.j == :y
@test ex == tref(:(a[1,y]))
y = 3
@test eval(ju_expr(ex)) == a[1,y]

ex = texpr(:(a[x,y]))
@test isa(ex, TGeneralRef2)
@test ex.host == tvar(:a)
@test ex.i == :x
@test ex.j == :y
@test ex == tref(:(a[x,y]))
@test eval(ju_expr(ex)) == a[x,y]

a = A1(a)

ex = texpr(:(a.b[2]))
@test isa(ex, TGeneralRef1)
@test ex.host == tqvar(:(a.b))
@test ex.i == 2
@test ex == tref(:(a.b[2]))
@test eval(ju_expr(ex)) == a.b[2]

ex = texpr(:(a.b[2,3]))
@test isa(ex, TGeneralRef2)
@test ex.host == tqvar(:(a.b))
@test ex.i == 2
@test ex.j == 3
@test ex == tref(:(a.b[2,3]))
@test eval(ju_expr(ex)) == a.b[2,3]

a = {"s" => 100}

ex = texpr(:(a["s"]))
@test isa(ex, TGeneralRef1)
@test ex.host == tvar(:a)
@test ex.i == "s"
@test ex == tref(:(a["s"]))
@test eval(ju_expr(ex)) == a["s"]

ex = texpr(:(a["s", "t"]))
@test isa(ex, TGeneralRef2)
@test ex.host == tvar(:a)
@test ex.i == "s"
@test ex.j == "t"
@test ex == tref(:(a["s", "t"]))


###########################################################
#
#	1D array reference expression construction
#
###########################################################

ex = texpr(:(a[:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TColon()
@test ex == tref(:(a[:]))

ex = texpr(:(a[2:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(2, nothing)
@test ex == tref(:(a[2:]))

ex = texpr(:(a[i0:]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, nothing)
@test ex == tref(:(a[i0:]))

ex = texpr(:(a[3:7]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(3, 7)
@test ex == tref(:(a[3:7]))

ex = texpr(:(a[i0:7]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, 7)
@test ex == tref(:(a[i0:7]))

ex = texpr(:(a[3:i1]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(3, :i1)
@test ex == tref(:(a[3:i1]))

ex = texpr(:(a[i0:i1]))
@test isa(ex, TRef1D)
@test ex.host == tvar(:a)
@test ex.rgn == TInterval(:i0, :i1)
@test ex == tref(:(a[i0:i1]))

ex = texpr(:(a.b[:]))
@test isa(ex, TRef1D)
@test ex.host == tqvar(:(a.b))
@test ex.rgn == TColon()
@test ex == tref(:(a.b[:]))


###########################################################
#
#	column reference expression construction
#
###########################################################

ex = texpr(:(a[:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.icol == 5
@test ex == tref(:(a[:, 5]))

ex = texpr(:(a[2:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(2, nothing)
@test ex.icol == 5
@test ex == tref(:(a[2:, 5]))

ex = texpr(:(a[i0:, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.icol == 5
@test ex == tref(:(a[i0:, 5]))

ex = texpr(:(a[3:7, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, 7)
@test ex.icol == 5
@test ex == tref(:(a[3:7, 5]))

ex = texpr(:(a[i0:7, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, 7)
@test ex.icol == 5
@test ex == tref(:(a[i0:7, 5]))

ex = texpr(:(a[3:i1, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, :i1)
@test ex.icol == 5
@test ex == tref(:(a[3:i1, 5]))

ex = texpr(:(a[i0:i1, 5]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.icol == 5
@test ex == tref(:(a[i0:i1, 5]))


ex = texpr(:(a[:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.icol == :j
@test ex == tref(:(a[:, j]))

ex = texpr(:(a[2:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(2, nothing)
@test ex.icol == :j
@test ex == tref(:(a[2:, j]))

ex = texpr(:(a[i0:, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.icol == :j
@test ex == tref(:(a[i0:, j]))

ex = texpr(:(a[3:7, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, 7)
@test ex.icol == :j
@test ex == tref(:(a[3:7, j]))

ex = texpr(:(a[i0:7, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, 7)
@test ex.icol == :j
@test ex == tref(:(a[i0:7, j]))

ex = texpr(:(a[3:i1, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(3, :i1)
@test ex.icol == :j
@test ex == tref(:(a[3:i1, j]))

ex = texpr(:(a[i0:i1, j]))
@test isa(ex, TRefCol)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.icol == :j
@test ex == tref(:(a[i0:i1, j]))

ex = texpr(:(a.b[:,j]))
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

ex = texpr(:(a[6, :]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TColon()
@test ex == tref(:(a[6, :]))

ex = texpr(:(a[6, 2:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[6, 2:]))

ex = texpr(:(a[6, i0:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, nothing)
@test ex == tref(:(a[6, i0:]))

ex = texpr(:(a[6, 3:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[6, 3:7]))

ex = texpr(:(a[6, i0:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, 7)
@test ex == tref(:(a[6, i0:7]))

ex = texpr(:(a[6, 3:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(3, :i1)
@test ex == tref(:(a[6, 3:i1]))

ex = texpr(:(a[6, i0:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == 6
@test ex.crgn == TInterval(:i0, :i1)
@test ex == tref(:(a[6, i0:i1]))

ex = texpr(:(a[i, :]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TColon()
@test ex == tref(:(a[i, :]))

ex = texpr(:(a[i, 2:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i, 2:]))

ex = texpr(:(a[i, i0:]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, nothing)
@test ex == tref(:(a[i, i0:]))

ex = texpr(:(a[i, 3:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i, 3:7]))

ex = texpr(:(a[i, i0:7]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, 7)
@test ex == tref(:(a[i, i0:7]))

ex = texpr(:(a[i, 3:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(3, :i1)
@test ex == tref(:(a[i, 3:i1]))

ex = texpr(:(a[i, i0:i1]))
@test isa(ex, TRefRow)
@test ex.host == tvar(:a)
@test ex.irow == :i
@test ex.crgn == TInterval(:i0, :i1)
@test ex == tref(:(a[i, i0:i1]))

ex = texpr(:(a.b[i, :]))
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

ex = texpr(:(a[:, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TColon()
@test ex == tref(:(a[:, :]))

ex = texpr(:(a[:, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[:, 2:]))

ex = texpr(:(a[:, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[:, j0:]))

ex = texpr(:(a[:, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[:, 3:7]))

ex = texpr(:(a[:, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[:, j0:7]))

ex = texpr(:(a[:, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[:, 3:j1]))

ex = texpr(:(a[:, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TColon()
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[:, j0:j1]))


ex = texpr(:(a[i0:, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TColon()
@test ex == tref(:(a[i0:, :]))

ex = texpr(:(a[i0:, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i0:, 2:]))

ex = texpr(:(a[i0:, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[i0:, j0:]))

ex = texpr(:(a[i0:, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i0:, 3:7]))

ex = texpr(:(a[i0:, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[i0:, j0:7]))

ex = texpr(:(a[i0:, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[i0:, 3:j1]))

ex = texpr(:(a[i0:, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, nothing)
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[i0:, j0:j1]))


ex = texpr(:(a[i0:i1, :]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TColon()
@test ex == tref(:(a[i0:i1, :]))

ex = texpr(:(a[i0:i1, 2:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(2, nothing)
@test ex == tref(:(a[i0:i1, 2:]))

ex = texpr(:(a[i0:i1, j0:]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, nothing)
@test ex == tref(:(a[i0:i1, j0:]))

ex = texpr(:(a[i0:i1, 3:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(3, 7)
@test ex == tref(:(a[i0:i1, 3:7]))

ex = texpr(:(a[i0:i1, j0:7]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, 7)
@test ex == tref(:(a[i0:i1, j0:7]))

ex = texpr(:(a[i0:i1, 3:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(3, :j1)
@test ex == tref(:(a[i0:i1, 3:j1]))

ex = texpr(:(a[i0:i1, j0:j1]))
@test isa(ex, TRef2D)
@test ex.host == tvar(:a)
@test ex.rrgn == TInterval(:i0, :i1)
@test ex.crgn == TInterval(:j0, :j1)
@test ex == tref(:(a[i0:i1, j0:j1]))

ex = texpr(:(a.b[i0:i1, j0:j1]))
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

ex = texpr(:(sin(a)))
@test isa(ex, TMap)
@test ex.fun == :sin
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)
@test ex == tcall(:(sin(a)))

ex = texpr(:(a + b))
@test isa(ex, TMap)
@test ex.fun == :(+)
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)
@test ex == tcall(:(a + b))

ex = texpr(:(clamp(a, b, c)))
@test isa(ex, TMap)
@test ex.fun == :clamp
@test length(ex.args) == 3
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)
@test ex.args[3] == tvar(:c)
@test ex == tcall(:(clamp(a, b, c)))

ex = texpr(:(a + b .* c))
@test isa(ex, TMap)
@test ex.fun == :+
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tcall(:(b .* c))

ex = texpr(:(a.x + max(b[2], c[:,i])))
@test isa(ex, TMap)
@test ex.fun == :+
@test length(ex.args) == 2
@test ex.args[1] == tqvar(:(a.x))
@test ex.args[2].fun == :max
@test ex.args[2].args[1] == tref(:(b[2]))
@test ex.args[2].args[2] == tref(:(c[:,i]))

# reductions

ex = texpr(:(sum(a)))
@test isa(ex, TReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(max(a)))
@test isa(ex, TReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(min(a)))
@test isa(ex, TReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(mean(a)))
@test isa(ex, TReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(dot(a, b)))
@test isa(ex, TReduc)
@test ex.fun == :dot
@test length(ex.args) == 2
@test ex.args[1] == tvar(:a)
@test ex.args[2] == tvar(:b)

ex = texpr(:(sum(a, 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(max(a, (), 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(min(a, (), 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(mean(a, 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(sum(a, 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(max(a, (), 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :max
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(min(a, (), 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :min
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)

ex = texpr(:(mean(a, 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :mean
@test length(ex.args) == 1
@test ex.args[1] == tvar(:a)


ex = texpr(:(sum(a[1,2] + b .* c[:,:])))
@test isa(ex, TReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))

ex = texpr(:(sum(a[1,2] + b .* c[:,:], 1)))
@test isa(ex, TColwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))

ex = texpr(:(sum(a[1,2] + b .* c[:,:], 2)))
@test isa(ex, TRowwiseReduc)
@test ex.fun == :sum
@test length(ex.args) == 1
@test ex.args[1] == tcall(:(a[1,2] + b .* c[:,:]))


###########################################################
#
#	Assignments and blocks
#
###########################################################

# assignment

ex = texpr(:(a = b))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:a)
@test ex.rhs == tvar(:b)
@test is_trivial_assignment(ex)
@test ex == tassign(:(a = b))

ex = texpr(:(r.a = b))
@test isa(ex, TAssign)
@test ex.lhs == tqvar(:(r.a))
@test ex.rhs == tvar(:b)
@test is_trivial_assignment(ex)
@test ex == tassign(:(r.a = b))

ex = texpr(:(r.a = s.b))
@test isa(ex, TAssign)
@test ex.lhs == tqvar(:(r.a))
@test ex.rhs == tqvar(:(s.b))
@test is_trivial_assignment(ex)
@test ex == tassign(:(r.a = s.b))

ex = texpr(:(a = b + c))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:a)
@test ex.rhs == tcall(:(b + c))
@test !is_trivial_assignment(ex)
@test ex == tassign(:(a = b + c))

ex = texpr(:(r.a[:,1] = sin(a) + b[:]))
@test isa(ex, TAssign)
@test ex.lhs == tref(:(r.a[:,1]))
@test ex.rhs == tcall(:(sin(a) + b[:]))
@test !is_trivial_assignment(ex)
@test ex == tassign(:(r.a[:,1] = sin(a) + b[:]))


# op-assignment

ex = texpr(:(r += a))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:r)
@test ex.rhs == tcall(:(r + a))
@test ex == tassign(:(r = r + a))

ex = texpr(:(r.x -= a.y))
@test isa(ex, TAssign)
@test ex.lhs == tqvar(:(r.x))
@test ex.rhs == tcall(:(r.x - a.y))
@test ex == tassign(:(r.x = r.x - a.y))

ex = texpr(:(r .*= a + b))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:r)
@test ex.rhs == tcall(:(r .* (a + b)))
@test ex == tassign(:(r = r .* (a + b)))

ex = texpr(:(r ./= a + b))
@test isa(ex, TAssign)
@test ex.lhs == tvar(:r)
@test ex.rhs == tcall(:(r ./ (a + b)))
@test ex == tassign(:(r = r ./ (a + b)))


# tblock

blk = quote
	a = b + 1
	c.x[:] = a[:,j]
	c .*= 2
end
ex = texpr(blk)
@test isa(ex, TBlock)
@test length(ex.stmts) == 3
@test ex.stmts[1] == tassign(:(a = b + 1))
@test ex.stmts[2] == tassign(:(c.x[:] = a[:,j]))
@test ex.stmts[3] == tassign(:(c = c .* 2))
@test ex == tblock(blk)






