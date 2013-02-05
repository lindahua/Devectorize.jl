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

# tvar

ex = tvar(:a)
@test isa(ex, TVar)
@test isa(ex.form, Symbol)
@test ex.form == :(a)

ex = tvar(:(a.b))
@test isa(ex, TVar)
@test isa(ex.form, Expr)
@test ex.form == :(a.b)

ex = tvar(:(a.b.c))
@test isa(ex, TVar)
@test isa(ex.form, Expr)
@test ex.form == :(a.b.c)

# tscalarvar

ex = tscalarvar(:a)
@test isa(ex, TScalarVar)
@test ex.name == (:a)

###########################################################
#
#	reference expression construction
#
###########################################################

ex = tref(:(a[1]))
@test isa(ex, TRefScalar1)
@test ex.host == tvar(:a)
@test ex.i == 1

ex = tref(:(a[1,2]))
@test isa(ex, TRefScalar2)
@test isa(ex.host, TVar)
@test ex.host.form == :a
@test ex.i == 1
@test ex.j == 2

ex = tref(:(a[:]))
@test isa(ex, TRef1D)
@test isa(ex.host, TVar)
@test ex.host.form == :a
@test ex.rgn == TColon()

ex = tref(:(a[:,:]))
@test isa(ex, TRef2D)
@test isa(ex.host, TVar)
@test ex.host.form == :a
@test ex.rrgn == :(:)
@test ex.crgn == :(:)

ex = tref(:(a[:,j]))
@test isa(ex, TRefCol)
@test isa(ex.host, TVar)
@test ex.host.form == :a
@test ex.rrgn == TColon()
@test ex.icol == (:j)

ex = tref(:(a[i,:]))
@test isa(ex, TRefRow)
@test isa(ex.host, TVar)
@test ex.host.form == :a
@test ex.irow == (:i)
@test ex.crgn == TColon()


###########################################################
#
#	expression types and modes
#
###########################################################

macro test_te(expr, expect_ty, expect_mode)
	te = texpr(expr)
	if !isa(te, eval(expect_ty))
		error("texpr failed on: texpr_type of ($expr) == $(eval(expect_ty)) | got $(typeof(te))")
	end
	if !(tmode(te) == eval(expect_mode))
		error("texpr failed on: tmode of ($expr) == $(eval(expect_mode)) | got $(tmode(te))")
	end
end

macro expect_error(expr, errty)
	last_err = nothing
	try
		texpr(expr)
	catch err
		last_err = err
	end
	if last_err == nothing
		error("expect exception from $expr, but it raises nothing (unexpectedly)")
	elseif !isa(last_err, eval(errty))
		error("The expression $expr raises unexpected exception of $(typeof(last_err))")
	end
end

# terminal

@test_te 1 		TNum{Int} 		ScalarMode()
@test_te 2.5 	TNum{Float64}	ScalarMode()
@test_te a 		TSym 			EWiseMode{0}()

# reference

@test_te a[1]	TRefScalar1		ScalarMode()
@test_te a[i] 	TRefScalar1 	ScalarMode()
@test_te a[:] 	TRef1D 			EWiseMode{1}()
@test_te a[:,1]	TRefCol 		EWiseMode{1}()
@test_te a[:,j]	TRefCol			EWiseMode{1}()
@test_te a[1,:] TRefRow 		EWiseMode{1}()
@test_te a[i,:] TRefRow 		EWiseMode{1}()
@test_te a[:,:] TRef2D 			EWiseMode{2}()

# unary expressions

@test_te -a 			TMap 	EWiseMode{0}()
@test_te sin(1.2) 		TMap 	ScalarMode()
@test_te sin(a) 		TMap 	EWiseMode{0}()
@test_te sin(a[0]) 		TMap 	ScalarMode()
@test_te sin(a[:])	 	TMap 	EWiseMode{1}()
@test_te sin(a[:,:]) 	TMap 	EWiseMode{2}()

# binary expressions

@test_te a + 1 			TMap	EWiseMode{0}()
@test_te a + b			TMap 	EWiseMode{0}()
@test_te a + b[1] 		TMap	EWiseMode{0}()
@test_te a + b[:] 		TMap	EWiseMode{1}()
@test_te a + b[:,:]		TMap 	EWiseMode{2}()

@test_te 1 + 2 			TMap	ScalarMode()
@test_te 1 + b			TMap 	EWiseMode{0}()
@test_te 1 + b[1] 		TMap	ScalarMode()
@test_te 1 + b[:] 		TMap	EWiseMode{1}()
@test_te 1 + b[:,:]		TMap 	EWiseMode{2}()

@test_te a[1] + 2 			TMap	ScalarMode()
@test_te a[1] + b			TMap 	EWiseMode{0}()
@test_te a[1] + b[1] 		TMap	ScalarMode()
@test_te a[1] + b[:] 		TMap	EWiseMode{1}()
@test_te a[1] + b[:,:]		TMap 	EWiseMode{2}()

@test_te a[:] + 1 			TMap	EWiseMode{1}()
@test_te a[:] + b			TMap 	EWiseMode{1}()
@test_te a[:] + b[1] 		TMap	EWiseMode{1}()
@test_te a[:] + b[:] 		TMap	EWiseMode{1}()
@expect_error a[:] + b[:,:]   DeError

@test_te a[:,:] + 1 		TMap	EWiseMode{2}()
@test_te a[:,:] + b			TMap 	EWiseMode{2}()
@test_te a[:,:] + b[1] 		TMap	EWiseMode{2}()
@test_te a[:,:] + b[:,:] 	TMap 	EWiseMode{2}()
@expect_error a[:,:] + b[:]   DeError

# ternary expressions

@test_te clamp(a, b, c) 		TMap 	EWiseMode{0}()
@test_te clamp(1, b, c) 		TMap 	EWiseMode{0}()
@test_te clamp(a, 2, c) 		TMap 	EWiseMode{0}()
@test_te clamp(a, b, 3) 		TMap 	EWiseMode{0}()
@test_te clamp(1, 2, c) 		TMap 	EWiseMode{0}()
@test_te clamp(1, b, 3) 		TMap 	EWiseMode{0}()
@test_te clamp(a, 2, 3) 		TMap 	EWiseMode{0}()
@test_te clamp(1, 2, 3) 		TMap 	ScalarMode()

@test_te clamp(a[:], b, c) 		TMap 	EWiseMode{1}()
@test_te clamp(a, b[:], c) 		TMap 	EWiseMode{1}()
@test_te clamp(a, b, c[:]) 		TMap 	EWiseMode{1}()
@test_te clamp(a[:], b[:], c) 	TMap 	EWiseMode{1}()
@test_te clamp(a[:], b, c[:]) 	TMap 	EWiseMode{1}()
@test_te clamp(a, b[:], c[:]) 	TMap 	EWiseMode{1}()
@test_te clamp(a[:], b[:], c[:]) 	TMap 	EWiseMode{1}()

@test_te clamp(a[:], 1, 2) 		TMap 	EWiseMode{1}()
@test_te clamp(1, b[:], 2) 		TMap 	EWiseMode{1}()
@test_te clamp(1, 2, c[:]) 		TMap 	EWiseMode{1}()
@test_te clamp(a[:], b[:], 1) 	TMap 	EWiseMode{1}()
@test_te clamp(a[:], 1, c[:]) 	TMap 	EWiseMode{1}()
@test_te clamp(1, b[:], c[:]) 	TMap 	EWiseMode{1}()

# comparison & logical expressions

@test_te a .== b 		TMap	EWiseMode{0}()
@test_te 1 .== a 		TMap 	EWiseMode{0}()
@test_te a .== 1 		TMap 	EWiseMode{0}()
@test_te 1 .== 2 		TMap 	ScalarMode()

@test_te a .!= b 		TMap 	EWiseMode{0}()
@test_te a .< b 		TMap 	EWiseMode{0}()
@test_te a .> b 		TMap 	EWiseMode{0}()
@test_te a .<= b 		TMap 	EWiseMode{0}()
@test_te a .>= b 		TMap 	EWiseMode{0}()

@test_te a & b 			TMap	EWiseMode{0}()
@test_te a & true 		TMap	EWiseMode{0}()
@test_te true & a 		TMap	EWiseMode{0}()
@test_te true & false	TMap 	ScalarMode()

@test_te a | b 			TMap	EWiseMode{0}()
@test_te a | true 		TMap	EWiseMode{0}()
@test_te true | a 		TMap	EWiseMode{0}()
@test_te true | false	TMap 	ScalarMode()


# compound ewise expressions

@test_te a + b .* c 	TMap 	EWiseMode{0}()
@test_te 1 + 2 .* 3 	TMap 	ScalarMode()
@test_te a + 2 .* 3 	TMap	EWiseMode{0}()
@test_te 1 + 2 .* b 	TMap 	EWiseMode{0}()

@test_te a[:] + b .* 2 		TMap	EWiseMode{1}()
@test_te a + b[:] .* c 		TMap	EWiseMode{1}()
@test_te a + 3 .* c[:,:]	TMap	EWiseMode{2}()
@test_te a[0] + 2 .* c[0]	TMap 	ScalarMode()

@test_te blend(a .> 1, c, d)	TMap 	EWiseMode{0}()


# reduction expressions

@test_te sum(a) 	TReduc 	ReducMode()
@test_te mean(a) 	TReduc 	ReducMode()
@test_te min(a) 	TReduc 	ReducMode()
@test_te max(a)		TReduc  ReducMode()
@test_te dot(a, b) 	TReduc  ReducMode()

@test_te sum(1) 	TReduc  ScalarMode()
@test_te mean(1) 	TReduc 	ScalarMode()
@test_te min(1) 	TReduc 	ScalarMode()
@test_te max(1)		TReduc  ScalarMode()
@test_te dot(1, 2) 	TReduc  ScalarMode()

@test_te sum(a[:]) 		TReduc 	ReducMode()
@test_te mean(a[:]) 	TReduc 	ReducMode()
@test_te min(a[:]) 		TReduc 	ReducMode()
@test_te max(a[:])		TReduc  ReducMode()
@test_te dot(a, b[:]) 	TReduc  ReducMode()

@test_te sum(a[:,:]) 		TReduc 	ReducMode()
@test_te mean(a[:,:]) 		TReduc 	ReducMode()
@test_te min(a[:,:]) 		TReduc 	ReducMode()
@test_te max(a[:,:])		TReduc  ReducMode()
@test_te dot(a, b[:,:]) 	TReduc  ReducMode()

@test_te sum(a + b) 	TReduc 	ReducMode()
@test_te mean(a + b) 	TReduc 	ReducMode()
@test_te min(a + b) 	TReduc 	ReducMode()
@test_te max(a + b)		TReduc  ReducMode()
@test_te dot(a + b, c .* d) 	TReduc  ReducMode()


# assignment expressions

@test_te a = 1 			TAssign{TSym, TNum{Int}} 	ScalarMode()
@test_te a = x			TAssign{TSym, TSym} 		EWiseMode{0}()
@test_te a = sin(x)		TAssign{TSym, TMap}			EWiseMode{0}()
@test_te a = x + y 		TAssign{TSym, TMap} 		EWiseMode{0}()
@test_te a = x + y[:]	TAssign{TSym, TMap} 		EWiseMode{1}()
@test_te a = x[:,:]		TAssign{TSym, TRef2D} 		EWiseMode{2}()

@test_te a[:] = 1 			TAssign{TRef1D, TNum{Int}}	EWiseMode{1}()
@test_te a[:] = x			TAssign{TRef1D, TSym} 		EWiseMode{1}()
@test_te a[:] = sin(x)		TAssign{TRef1D, TMap}		EWiseMode{1}()
@test_te a[:] = x + y 		TAssign{TRef1D, TMap} 		EWiseMode{1}()
@test_te a[:] = x + y[:]	TAssign{TRef1D, TMap} 		EWiseMode{1}()
@expect_error a[:] = x[:,:]		DeError

@test_te a[:,i] = 1 			TAssign{TRefCol, TNum{Int}}	EWiseMode{1}()
@test_te a[:,i] = x				TAssign{TRefCol, TSym} 		EWiseMode{1}()
@test_te a[:,i] = sin(x)		TAssign{TRefCol, TMap}		EWiseMode{1}()
@test_te a[:,i] = x + y 		TAssign{TRefCol, TMap} 		EWiseMode{1}()
@test_te a[:,i] = x + y[:]		TAssign{TRefCol, TMap} 		EWiseMode{1}()
@expect_error a[:,i] = x[:,:]	DeError

@test_te a[i,:] = 1 			TAssign{TRefRow, TNum{Int}}	EWiseMode{1}()
@test_te a[i,:] = x				TAssign{TRefRow, TSym} 		EWiseMode{1}()
@test_te a[i,:] = sin(x)		TAssign{TRefRow, TMap}		EWiseMode{1}()
@test_te a[i,:] = x + y 		TAssign{TRefRow, TMap} 		EWiseMode{1}()
@test_te a[i,:] = x + y[:]		TAssign{TRefRow, TMap} 		EWiseMode{1}()
@expect_error a[i,:] = x[:,:]	DeError

@test_te a[:,:] = 1 			TAssign{TRef2D, TNum{Int}}	EWiseMode{2}()
@test_te a[:,:] = x				TAssign{TRef2D, TSym} 		EWiseMode{2}()
@test_te a[:,:] = sin(x)		TAssign{TRef2D, TMap}		EWiseMode{2}()
@test_te a[:,:] = x + y 		TAssign{TRef2D, TMap} 		EWiseMode{2}()	
@test_te a[:,:] = x[:,:] 		TAssign{TRef2D, TRef2D} 	EWiseMode{2}()
@expect_error a[:,:] = x + y[:] DeError


