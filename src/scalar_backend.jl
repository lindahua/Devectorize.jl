
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

function reduc_computation(f::TFun{:(sum)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( zero($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s += $(xs[1]) )
	post = quote end
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(max)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( typemin($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s = max($s, $(xs[1])) )
	post = quote end
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(min)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( typemax($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s = min($s, $(xs[1])) )
	post = quote end
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(mean)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( zero($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s += $(xs[1]) )
	post = :( $s /= $n )
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(dot)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( zero($ty) )
	init_val = :( $(xs[1]) * $(xs[2]) )
	updater = :( $s += $(xs[1]) * $(xs[2]) )
	post = quote end
	(empty_val, init_val, updater, post)
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

	@gensym tmp ty siz

	s = isa(ex.lhs, TSym) ? ex.lhs.e : tmp

	code = code_block(
		assignment(ty, type_inference(ex)),
		assignment(siz, args_size_inference(ex.rhs.args))
	)
	(code, (s, siz, ty))
end

function compose_init(ctx::ScalarContext, mode::PReducMode, ex::TAssign)

	@gensym siz ty

	r = ex.rhs

	if isa(ex.lhs, TSym)
		code = quote
			($ty) = $(type_inference(ex))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
			if $(r.dim) == 1
				$(ex.lhs.e) = Array(($ty), (1, ($siz)[2]))
			elseif $(r.dim) == 2
				$(ex.lhs.e) = Array(($ty), (($siz)[1],))
			else
				throw(DeExpr.DeError("DeExpr supports partial reduction along dim 1 or 2."))
			end
		end
	else
		code = quote
			($ty) = $(type_inference(ex.lhs))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
			if length($siz) == 1
				($siz) = (($siz)[1], 1)
			end
			if !( ($(r.dim) == 1) || ($(r.dim) == 2) )
				throw(DeExpr.DeError("DeExpr supports partial reduction along dim 1 or 2."))
			end
		end
	end

	(code, (siz, ty))
end


##########################################################################
#
# 	main body compilation
#
##########################################################################

# Element-wise

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

# Full reduction

function compose_reduc_core(ctx::ScalarContext, mode::EWiseMode, r::TReduc, 
	ty::Symbol, s::Symbol, n::Symbol, idxinfo...)

	@gensym x1 x2

	na = length(r.args)
	tfun = TFun{r.fun}()

	if na == 1
		arg1_pre, arg1_calc = compose(ctx, mode, r.args[1], idxinfo...)
		rempty, rinit, rupdate, rpost = reduc_computation(tfun, ty, s, n, x1)
		arg_pre = arg1_pre
		arg_calc = assignment(x1, arg1_calc)
	elseif na == 2
		arg1_pre, arg1_calc = compose(ctx, mode, r.args[1], idxinfo...)
		arg2_pre, arg2_calc = compose(ctx, mode, r.args[2], idxinfo...)
		rempty, rinit, rupdate, rpost = reduc_computation(tfun, ty, s, n, x1, x2)
		arg_pre = code_block(arg1_pre, arg2_pre)
		arg_calc = code_block(
			assignment(x1, arg1_calc),
			assignment(x2, arg2_calc)
		)
	end

	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost)
end


function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{1}, l::TExpr, r::TReduc, info)
	@gensym n i
	s, siz, ty = info

	# computation kernels

	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
		ctx, mode, r, ty, s, n, i)

	# store back statement

	if isa(l, TSym)
		store_back = quote end
	else
		store_back = :( $(compose(ctx, ScalarMode(), l)) = ($s) ) 
	end

	flatten_code_block(
		assignment(n, fun_call(qname(:to_length), siz)),
		arg_pre,
		assignment(s, rempty),
		for_statement( :( ($i) = 1 : ($n) ), code_block(
			arg_calc,
			rupdate
		) ),
		rpost,
		store_back
	)
end


function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{2}, l::TExpr, r::TReduc, info)
	@gensym len m n i j
	s, siz, ty = info

	# computation kernels

	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
		ctx, mode, r, ty, s, len, i, j)

	# store back statement

	if isa(l, TSym)
		store_back = quote end
	else
		store_back = :( $(compose(ctx, ScalarMode(), l)) = ($s) ) 
	end

	# major block (for where n > 0)

	mblock = flatten_code_block(
		arg_pre,
		assignment(s, rempty),
		for_statement( :( ($j) = 1 : ($n) ), code_block(
			for_statement( :( ($i) = 1 : ($m) ), code_block(
				arg_calc,
				rupdate
			) )
		) ),
		rpost
	)

	# wrap up 

	flatten_code_block(
		if_statement( :(length($siz) == 1), 
			code_block(
				assignment(m, :(($siz)[1]) ),
				assignment(n, :(1) ),
				assignment(len, m),
			),
			code_block(
				assignment(m, :(($siz)[1]) ),
				assignment(n, :(($siz)[2]) ),
				assignment(len, fun_call(:*, m, n)),
			)
		),
		if_statement( :( ($len) > 0 ),
			mblock,
			assignment(s, rempty)
		),
		store_back
	)
end


function compose_main(ctx::ScalarContext, mode::ReducMode, ex::TAssign, info)
	lhs = ex.lhs
	rhs = ex.rhs

	rmode = rhs.arg_mode
	if isa(rmode, EWiseMode{0})
		rmode = EWiseMode{1}()
	end

	main = compose_reduc_main(ctx, rmode, lhs, rhs, info)
end


function compose_main(ctx::ScalarContext, mode::PReducMode, ex::TAssign, info)
	lhs = ex.lhs
	rhs = ex.rhs
	@assert length(rhs.args) == 1

	siz, ty = info

	@gensym m n i j s x1

	arg1_pre, arg1_calc = compose(ctx, EWiseMode{2}(), rhs.args[1], i, j)
	lhs_i_pre, lhs_i = compose_lhs(ctx, EWiseMode{1}(), lhs, i)
	lhs_j_pre, lhs_j = compose_lhs(ctx, EWiseMode{1}(), lhs, j)

	quote
		($m), ($n) = ($siz)
		if ($(rhs.dim) == 1)
			($lhs_j_pre)
			($arg1_pre)
			for ($j) = 1 : ($n)
				($s) = $(reduc_init(rhs.fun, ty))
				for ($i) = 1 : ($m)
					($x1) = ($arg1_calc)
					$(reduc_update(rhs.fun, s, x1))
				end
				($lhs_j) = ($s)
			end
		else  # rhs.dim == 2
		end
	end
end


# Fast full reduction

function compile_fast_reduc(ctx::ScalarContext, iex::TExpr, ex::TAssign)
	if !(isa(ex.lhs, TSym) && isa(ex.rhs, TReduc))
		throw(DeError("Invalid expression for fast_reduc."))
	end

	@gensym siz
	ty = iex.host

	siz_infer, mode = if isa(iex, TRefScalar1)
		:( ($siz) = ($(iex.i),) ), EWiseMode{1}()
	elseif isa(iex, TRefScalar2)
		:( ($siz) = ($(iex.i), $(iex.j)) ), EWiseMode{2}()
	else
		throw(DeError("Invalid size and type spec"))
	end

	s = ex.lhs.e
	main = compose_reduc_main(ctx, mode, ex.lhs, ex.rhs, (s, siz, ty))
	flatten_code_block(siz_infer, main)
end

function compile_fast_reduc(ctx::ScalarContext, iex::Expr, ex::Expr)
	compile_fast_reduc(ctx, texpr(iex), texpr(ex))
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

macro inspect_devec(assign_ex)
	let code__ = compile(ScalarContext(), assign_ex)
		println("$assign_ex ==>")
		println(code__)
	end
	esc(begin 
		compile(ScalarContext(), assign_ex)
	end)
end

macro fast_reduc(info_ex, assign_ex)
	esc(begin
		compile_fast_reduc(ScalarContext(), info_ex, assign_ex)
	end)
end

macro inspect_fast_reduc(info_ex, assign_ex)
	let code__ = compile_fast_reduc(ScalarContext(), info_ex, assign_ex)
		println("$assign_ex ==>")
		println(code__)
	end
	esc(begin
		compile_fast_reduc(ScalarContext(), info_ex, assign_ex)
	end)
end



