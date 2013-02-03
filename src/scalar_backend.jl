
type ScalarContext <: DirectContext  # de-vectorized scalar for-loop
end

##########################################################################
#
# 	array access types
#
##########################################################################

# const

type DeConst{T<:Real}
	val::T
end
get{T<:Number}(r::DeConst{T}, ::Int) = r.val
get{T<:Number}(r::DeConst{T}, ::Int, ::Int) = r.val

# vector reader

type DeArr{T<:Number}
	src::Array{T}
end
get{T<:Number}(r::DeArr{T}, i::Int) = r.src[i]
get{T<:Number}(r::DeArr{T}, i::Int, j::Int) = r.src[i,j]


# functions to generate accessors

de_arr{T<:Number}(v::T) = DeConst{T}(v)
de_arr{T<:Number}(a::Array{T}) = DeArr{T}(a)

# others

forward_number(x::Number) = x


##########################################################################
#
# 	scalar kernel
#
##########################################################################

function compose_scalar(ctx::ScalarContext, ex::TNum)
	pre = nothing
	kernel = :( $(ex.e) )
	(pre, kernel)
end

function compose_scalar(ctx::ScalarContext, ex::TScalarSym)
	pre = nothing
	kernel = :( $(ex.e) )
	(pre, kernel)
end

function compose_scalar(ctx::ScalarContext, ex::TRefScalar1)
	@gensym rv
	pre = assignment(rv, :( $(ex.host)[$(ex.i)] ))
	kernel = :( $rv )
	(pre, kernel)
end

function compose_scalar(ctx::ScalarContext, ex::TRefScalar2)
	@gensym rv
	pre = assignment(rv, :( $(ex.host)[$(ex.i), $(ex.j)] ))
	kernel = :( $rv )
	(pre, kernel)
end

function compose_scalar(ctx::ScalarContext, ex::TMap)
	@gensym rv

	arg_rs = [compose_scalar(ctx, a) for a in ex.args]

	arg_pres = [r[1] for r in arg_rs]
	arg_kers = [r[2] for r in arg_rs]

	pre = code_block(arg_pres..., 
		assignment(rv, fun_call(ex.fun, arg_kers...)))
	kernel = :( $(rv) )
	(pre, kernel)
end


compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TScalar,   
	i::Symbol) = compose_scalar(ctx, ex)

compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TScalar,   
	i::Symbol, j::Symbol) = compose_scalar(ctx, ex)


##########################################################################
#
# 	kernel composition
#
##########################################################################


# 1D EWise

# LHS

function compose_lhs(ctx::ScalarContext, mode::EWiseMode{1}, ex::TSym, i::Symbol)
	pre = nothing
	kernel = :( $(ex.e)[($i)] )
	(pre, kernel)
end

function compose_lhs(ctx::ScalarContext, mode::EWiseMode{1}, ex::TRef1D, i::Symbol)
	pre = nothing
	kernel = :( $(ex.host)[($i)] )
	(pre, kernel)
end

# RHS

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign, i::Symbol)
	lhs_pre, lhs_kernel = compose_lhs(ctx, mode, ex.lhs, i)
	rhs_pre, rhs_kernel = compose(ctx, mode, ex.rhs, i)

	pre = code_block(lhs_pre, rhs_pre)
	kernel = assignment(lhs_kernel, rhs_kernel)
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TSym, i::Symbol)
	@gensym rd
	pre = assignment(rd, fun_call(qname(:de_arr), ex.e))
	kernel = fun_call(qname(:get), rd, i)
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TRef1D, i::Symbol)
	pre = nothing
	kernel = :( $(ex.host)[$i] )
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TRefCol, i::Symbol)
	@gensym icol
	pre = :( $icol = convert(Int, $(ex.icol)) )
	kernel = :(  $(ex.host)[$i, $icol] )
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TRefRow, i::Symbol)
	@gensym irow
	pre = :( $irow = convert(Int, $(ex.irow)) )
	kernel = :( $(ex.host)[$irow, $i] )
	(pre, kernel)
end


# 2D EWise

# LHS

function compose_lhs(ctx::ScalarContext, mode::EWiseMode{2}, ex::TSym, i::Symbol, j::Symbol)
	pre = nothing
	kernel = :( $(ex.e)[($i), ($j)] )
	(pre, kernel)
end

function compose_lhs(ctx::ScalarContext, mode::EWiseMode{2}, ex::TRef2D, i::Symbol, j::Symbol)
	pre = nothing
	kernel = :( $(ex.host)[($i), ($j)] )
	(pre, kernel)
end

# RHS

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign, i::Symbol, j::Symbol)
	lhs_pre, lhs_kernel = compose_lhs(ctx, mode, ex.lhs, i, j)
	rhs_pre, rhs_kernel = compose(ctx, mode, ex.rhs, i, j)

	pre = code_block(lhs_pre, rhs_pre)
	kernel = assignment(lhs_kernel, rhs_kernel)
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TSym, i::Symbol, j::Symbol)
	@gensym rd
	pre = assignment(rd, fun_call(qname(:de_arr), ex.e))
	kernel = fun_call(qname(:get), rd, i, j)
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TRef2D, i::Symbol, j::Symbol)
	pre = nothing
	kernel = :( $(ex.host)[($i), ($j)] )
	(pre, kernel)
end


# Maps

function compose(ctx::ScalarContext, mode::EWiseMode, ex::TMap, sinfo...)
	@gensym rd

	if isa(ex.mode, ScalarMode)
		compose_scalar(ctx, ex)
	else
		arg_rs = [compose(ctx, mode, a, sinfo...) for a in ex.args]

		arg_pres = [r[1] for r in arg_rs]
		arg_kers = [r[2] for r in arg_rs]

		pre = code_block(arg_pres...)
		kernel = fun_call(ex.fun, arg_kers...)
		(pre, kernel)
	end
end

# Scalar case

compose(ctx::ScalarContext, mode::ScalarMode, ex::TNum) = :( $(ex.e) )
compose(ctx::ScalarContext, mode::ScalarMode, ex::TSym) = :( $(ex.e) )
compose(ctx::ScalarContext, mode::ScalarMode, ex::TScalarSym) = :( $(ex.e) )

compose(ctx::ScalarContext, mode::ScalarMode, ex::TRefScalar1) = :( $(ex.host)[$(ex.i)] )
compose(ctx::ScalarContext, mode::ScalarMode, ex::TRefScalar2) = :( $(ex.host)[$(ex.i), $(ex.j)] )

function compose(ctx::ScalarContext, mode::ScalarMode, ex::TMap)
	arg_calcs = [compose(ctx, mode, a) for a in ex.args]
	fun_call(ex.fun, arg_calcs...)
end

function compose(ctx::ScalarContext, mode::ScalarMode, ex::TAssign)
	l = compose(ctx, mode, ex.lhs)
	r = compose(ctx, mode, ex.rhs)
	:( ($l) = ($r) )
end


# Full reduction

function reduc_init(f::Symbol, ty::Symbol)
	f == (:sum)  ? :( zero($ty) ) :
	f == (:max)  ? :( typemin($ty) ) :
	f == (:min)  ? :( typemax($ty) ) :
	f == (:mean) ? :( zero($ty) ) :
	f == (:dot)  ? :( zero($ty) ) :
	throw(DeError("Unsupported reduction function $f"))
end

function reduc_update(f::Symbol, s::Symbol, args::Symbol...)
	f == (:sum)  ? :( $s += $(args[1]) ) :
	f == (:max)  ? :( $s = max($s, $(args[1])) ) :
	f == (:min)  ? :( $s = min($s, $(args[1])) ) :
	f == (:mean) ? :( $s += $(args[1]) ) :
	f == (:dot)  ? :( $s += $(args[1]) * $(args[2]) ) :
	throw(DeError("Unsupported reduction function $f"))
end


div_size(x::Real, n::Int) = x / n
div_size(x::Real, s::(Int,)) = x / s[1]
div_size(x::Real, s::(Int,Int)) = x / (s[1] * s[2])

function reduc_post(f::Symbol, s::Symbol, n::Symbol)
	f == (:mean) ? :( $s = DeExpr.div_size($s, $n) ) : (quote end)
end



##########################################################################
#
# 	init-part composition
#
##########################################################################

compose_init(ctx::ScalarContext, mode::ScalarMode, ex::TAssign) = (nothing, nothing)

function compose_init(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign)

	@gensym len siz ty

	if isa(ex.lhs, TSym)
		code = code_block(
			assignment(siz, size_inference(ex)),
			assignment(ty, type_inference(ex)),
			assignment(ex.lhs.e, fun_call(:Array, ty, siz)),
			assignment(len, length_getter(ex.lhs))
		)
	else
		code = assignment(len, length_getter(ex.lhs))
	end

	(code, len)
end

function compose_init(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign)

	@gensym siz ty

	if isa(ex.lhs, TSym)
		code = code_block(
			assignment(siz, size_inference(ex)),
			assignment(ty, type_inference(ex)),
			assignment(ex.lhs.e, fun_call(:Array, ty, siz))
		)
	else
		code = assignment(siz, size2d_getter(ex.lhs))
	end

	(code, siz)
end

function compose_init(ctx::ScalarContext, mode::ReducMode, ex::TAssign)

	@gensym tmp ty

	s = isa(ex.lhs, TSym) ? ex.lhs.e : tmp

	code = code_block(
		assignment(ty, type_inference(ex)),
		assignment(s, reduc_init(ex.rhs.fun, ty))
	)
	(code, s)
end



##########################################################################
#
# 	main body compilation
#
##########################################################################

compose_main(ctx::ScalarContext, mode::ScalarMode, ex::TAssign, ::Nothing) = compose(ctx, mode, ex)

function compose_main(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign, len::Symbol)

	@gensym i
	(pre, kernel) = compose(ctx, mode, ex, i)
	
	main_loop = quote
		for ($i) = 1 : ($len)
			($kernel)
		end
	end

	code_block(pre, main_loop)
end

function compose_main(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign, siz::Symbol)

	@gensym i j m n
	(pre, kernel) = compose(ctx, mode, ex, i, j)
	
	main_loop = quote
		($m) = ($siz)[1]
		($n) = ($siz)[2]
		for ($j) = 1 : ($n)
			for ($i) = 1 : ($m)
				($kernel)
			end
		end
	end

	code_block(pre, main_loop)
end


function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{1}, r::TReduc, s::Symbol)
	@gensym siz n i x1 x2

	na = length(r.args)

	if na == 1
		arg1_code = compose(ctx, mode, r.args[1], i)
		arg1_pre  = arg1_code[1]
		arg1_calc = arg1_code[2]
		quote
			($siz) = $(args_size_inference(r.args))
			($n) = prod($siz)
			($arg1_pre)
			for ($i) = 1 : ($n)
				($x1) = ($arg1_calc)
				$(reduc_update(r.fun, s, x1))
			end
			$(reduc_post(r.fun, s, n))
		end
	elseif na == 2
		arg1_code = compose(ctx, mode, r.args[1], i)
		arg2_code = compose(ctx, mode, r.args[2], i)
		arg1_pre  = arg1_code[1]
		arg2_pre  = arg2_code[1]
		arg1_calc = arg1_code[2]
		arg2_calc = arg2_code[2]
		quote
			($siz) = $(args_size_inference(r.args))
			($n) = prod($siz)
			($arg1_pre)
			($arg2_pre)
			for ($i) = 1 : ($n)
				($x1) = ($arg1_calc)
				($x2) = ($arg2_calc)
				$(reduc_update(r.fun, s, x1, x2))
			end
			$(reduc_post(r.fun, s, n))
		end
	end
end

function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{2}, r::TReduc, s::Symbol)
	@gensym siz m n i j x1 x2

	na = length(r.args)

	if na == 1
		arg1_code = compose(ctx, mode, r.args[1], i, j)
		arg1_pre  = arg1_code[1]
		arg1_calc = arg1_code[2]
		quote
			($siz) = $(args_size_inference(r.args))
			($m), $(n) = DeExpr.to_size2d(($siz))
			($arg1_pre)
			for ($j) = 1 : ($n), ($i) = 1 : ($m)
				($x1) = ($arg1_calc)
				$(reduc_update(r.fun, s, x1))
			end
			$(reduc_post(r.fun, s, siz))
		end
	elseif na == 2
		arg1_code = compose(ctx, mode, r.args[1], i, j)
		arg2_code = compose(ctx, mode, r.args[2], i, j)
		arg1_pre  = arg1_code[1]
		arg2_pre  = arg2_code[1]
		arg1_calc = arg1_code[2]
		arg2_calc = arg2_code[2]
		quote
			($siz) = $(args_size_inference(r.args))
			($m), $(n) = DeExpr.to_size2d(($siz))
			($arg1_pre)
			($arg2_pre)
			for ($j) = 1 : ($n), ($i) = 1 : ($m)
				($x1) = ($arg1_calc)
				($x2) = ($arg2_calc)
				$(reduc_update(r.fun, s, x1, x2))
			end
			$(reduc_post(r.fun, s, siz))
		end
	end
end

function compose_main(ctx::ScalarContext, mode::ReducMode, ex::TAssign, s::Symbol)
	rhs = ex.rhs

	rmode = rhs.arg_mode
	if isa(rmode, EWiseMode{0})
		rmode = EWiseMode{1}()
	end

	compose_reduc_main(ctx, rmode, rhs, s)
end

##########################################################################
#
# 	code-generating macros
#
##########################################################################

macro devec(assign_ex) 
	esc(begin 
		compile(ScalarContext(), assign_ex)
	end)
end

# macro to inspect the generated code

macro inspect_devec(assign_ex)
	let code = compile(ScalarContext(), assign_ex)
		println("$assign_ex ==>")
		println(code)
	end
	esc(begin 
		compile(ScalarContext(), assign_ex)
	end)
end

