
type ScalarContext <: DirectContext  # de-vectorized scalar for-loop
end

##########################################################################
#
# 	array access helpers
#
##########################################################################

# const

get_value(r::Number, ::Int) = r
get_value(r::Number, ::Int, ::Int) = r

# vector reader

get_value{T<:Number}(r::Array{T}, i::Int) = r[i]
get_value{T<:Number}(r::Array{T}, i::Int, j::Int) = r[i, j]
get_value{T<:Number}(r::AbstractArray{T}, i::Int) = r[i]
get_value{T<:Number}(r::AbstractArray{T}, i::Int, j::Int) = r[i, j]


##########################################################################
#
# 	Assignment composition
#
##########################################################################


function compose_kernel(ctx::ScalarContext, ex::TAssign, lhs_info, rhs_info, i::Symbol)

	lhs_kernel = compose_lhs_kernel(ctx, ex.lhs, lhs_info, i)
	rhs_kernel = compose_rhs_kernel(ctx, ex.rhs, rhs_info, i)
	assignment(lhs_kernel, rhs_kernel)
end

function compose_kernel(ctx::ScalarContext, ex::TAssign, lhs_info, rhs_info, i::Symbol, j::Symbol)

	lhs_kernel = compose_lhs_kernel(ctx, ex.lhs, lhs_info, i, j)
	rhs_kernel = compose_rhs_kernel(ctx, ex.rhs, rhs_info, i, j)
	assignment(lhs_kernel, rhs_kernel)
end


##########################################################################
#
# 	LHS composition
#
##########################################################################

# general setup routine for symbol replacement

function with_sym_replacement(ex::TGeneralVar)
	if isa(ex, TVar)
		(nothing, ex.name)
	else
		t = gensym("t")
		pre = assignment(t, ju_expr(ex))
		(pre, t)
	end
end


# TVar

function setup_lhs(ctx::ScalarContext, ex::TVar)
	(nothing, nothing)
end

function length_getter(ctx::ScalarContext, lhs::TVar, info::Nothing)
	fun_call(:length, lhs.name)
end

function size2d_getter(ctx::ScalarContext, lhs::TVar, info::Nothing)
	fun_call(qname(:to_size2d), fun_call(:size, lhs.name))
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol)
	:( $(ex.name)[$i] )
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol, j::Symbol)
	:( $(ex.name)[$i, $j] )
end

# TRef1D

function setup_lhs(ctx::ScalarContext, ex::TRef1D)
	with_sym_replacement(ex.host)
end

function length_getter(ctx::ScalarContext, lhs::TRef1D, h::Symbol)
	length_inference(h, lhs.rgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRef1D, h::Symbol, i::Symbol)
	:( $(h)[$(indexer(ex.rgn, i))] )
end

# TRefCol

function setup_lhs(ctx::ScalarContext, ex::TRefCol)
	(hpre, h) = with_sym_replacement(ex.host)
	if isa(ex.icol, Int)
		icol = ex.icol
		pre = hpre
	else
		icol = gensym("icol")
		pre = code_block(hpre, 
			assignment(icol, fun_call(:convert, :Int, ex.icol)) )
	end
	(pre, (h, icol))
end

function length_getter(ctx::ScalarContext, lhs::TRefCol, info)
	baselen = fun_call(:size, info[1], 1)
	length_inference_(baselen, lhs.rrgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRefCol, info, i::Symbol)
	h, icol = info
	:( $(h)[$(indexer(ex.rrgn, i)), $icol] )
end

# TRefRow

function setup_lhs(ctx::ScalarContext, ex::TRefRow)
	(hpre, h) = with_sym_replacement(ex.host)
	if isa(ex.irow, Int)
		irow = ex.irow
		pre = hpre
	else
		irow = gensym("irow")
		pre = code_block(hpre, 
			assignment(irow, fun_call(:convert, :Int, ex.irow)) )
	end
	(pre, (h, irow))
end

function length_getter(ctx::ScalarContext, lhs::TRefRow, info)
	baselen = fun_call(:size, info[1], 2)
	length_inference_(baselen, lhs.crgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRefRow, info, i::Symbol)
	h, irow = info
	:( $(h)[$irow, $(indexer(ex.crgn, i))] )
end

# TRef2D

function setup_lhs(ctx::ScalarContext, ex::TRef2D)
	with_sym_replacement(ex.host)
end

function size2d_getter(ctx::ScalarContext, lhs::TRef2D, h::Symbol)
	mat_size_inference(h, lhs.rrgn, lhs.crgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRef2D, h::Symbol, i::Symbol, j::Symbol)
	:( $(h)[$(indexer(ex.rrgn, i)), $(indexer(ex.crgn, j))] )
end


##########################################################################
#
# 	RHS composition
#
##########################################################################

# TNum

function setup_rhs(ctx::ScalarContext, ex::TNum)
	init = nothing
	siz = :()
	ty = fun_call(:typeof, ex.val)
	final = nothing
	info = nothing
	(init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TNum, info::Nothing, i::Symbol)
	ex.val
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TNum, info::Nothing, i::Symbol, j::Symbol)
	ex.val
end

# TVar

function setup_rhs(ctx::ScalarContext, ex::TVar)
	init = nothing
	siz = fun_call(:size, ex.name)
	ty = fun_call(:eltype, ex.name)
	final = nothing
	info = nothing
	(init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol)
	fun_call(qname(:get_value), ex.name, i)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol, j::Symbol)
	fun_call(qname(:get_value), ex.name, i, j)
end

# TScalarVar

function setup_rhs(ctx::ScalarContext, ex::TScalarVar)
	init = nothing
	siz = :()
	ty = fun_call(:typeof, ex.name)
	final = nothing
	info = nothing
	(init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TScalarVar, info::Nothing, i::Symbol)
	ex.name
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TScalarVar, info::Nothing, i::Symbol, j::Symbol)
	ex.name
end

# other TGeneralVar (e.g. TGeneralRef1, TGeneralRef2)

function setup_rhs(ctx::ScalarContext, ex::TGeneralVar)
	t = gensym("t")
	init = assignment(t, ju_expr(ex))
	siz = fun_call(:size, t)
	ty = fun_call(:eltype, t)
	final = nothing
	(init, siz, ty, final, t)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TGeneralVar, t::Symbol, i::Symbol)
	fun_call(qname(:get_value), t, i)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TGeneralVar, t::Symbol, i::Symbol, j::Symbol)
	fun_call(qname(:get_value), t, i, j)
end


# TRef1D, TRefCol, TRefRow

function setup_rhs(ctx::ScalarContext, ex::TRef1D)
	(init, h) = setup_lhs(ctx, ex)
	siz = vec_size_inference(h, ex.rgn)
	ty = fun_call(:eltype, h)
	final = nothing
	(init, siz, ty, final, h)
end

function setup_rhs(ctx::ScalarContext, ex::TRefCol)
	(init, info) = setup_lhs(ctx, ex)
	h = info[1]
	siz = col_size_inference(h, ex.rrgn)
	ty = fun_call(:eltype, h)
	final = nothing
	(init, siz, ty, final, info)
end

function setup_rhs(ctx::ScalarContext, ex::TRefRow)
	(init, info) = setup_lhs(ctx, ex)
	h = info[1]
	siz = row_size_inference(h, ex.crgn)
	ty = fun_call(:eltype, h)
	final = nothing
	(init, siz, ty, final, info)
end

compose_rhs_kernel(ctx::ScalarContext, ex::Union(TRef1D, TRefCol, TRefRow), info, 
	i::Symbol) = compose_lhs_kernel(ctx, ex, info, i)


# TRef2D

function setup_rhs(ctx::ScalarContext, ex::TRef2D)
	(init, h) = setup_lhs(ctx, ex)
	siz = mat_size_inference(h, ex.rrgn, ex.crgn)
	ty = fun_call(:eltype, h)
	final = nothing
	(init, siz, ty, final, h)
end

compose_rhs_kernel(ctx::ScalarContext, ex::TRef2D, h::Symbol, 
	i::Symbol, j::Symbol) = compose_lhs_kernel(ctx, ex, h, i, j)


# TMap

function setup_rhs(ctx::ScalarContext, ex::TMap)
	
	# setup arguments
	
	arg_setups = [setup_rhs(ctx, a) for a in ex.args]
	
	arg_inits = [s[1] for s in arg_setups]
	arg_sizes = [s[2] for s in arg_setups]
	arg_types = [s[3] for s in arg_setups]
	arg_finals = [s[4] for s in arg_setups]
	arg_infos = [s[5] for s in arg_setups]
	
	# integrate
	
	init = code_block(arg_inits...)
	siz = ewise_size_inference(arg_sizes...)
	ty = result_type_inference(ex.fun, arg_types...)
	final = code_block(arg_finals...)
	
	(init, siz, ty, final, arg_infos)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TMap, arg_infos, i::Symbol)
	na = length(ex.args)
	arg_kernels = [compose_rhs_kernel(ctx, ex.args[ia], arg_infos[ia], i) for ia in 1 : na]
	fun_call(ex.fun, arg_kernels...)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TMap, arg_infos, i::Symbol, j::Symbol)
	na = length(ex.args)
	arg_kernels = [compose_rhs_kernel(ctx, ex.args[ia], arg_infos[ia], i, j) for ia in 1 : na]
	fun_call(ex.fun, arg_kernels...)
end


# Full reduction

function reduc_computation(f::TFun{:(sum)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( zero($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s += $(xs[1]) )
	post = nothing
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(max)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( typemin($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s = max($s, $(xs[1])) )
	post = nothing
	(empty_val, init_val, updater, post)
end

function reduc_computation(f::TFun{:(min)}, ty::Symbol, s::Symbol, n::Symbol, xs::Symbol...)
	empty_val = :( typemax($ty) )
	init_val = :( $(xs[1]) )
	updater = :( $s = min($s, $(xs[1])) )
	post = nothing
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
	post = nothing
	(empty_val, init_val, updater, post)
end



##########################################################################
#
# 	main body compilation
#
##########################################################################

# Element-wise 1D

function compile(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign)

	lhs = ex.lhs
	rhs = ex.rhs

	@assert(isa(lhs, TVar) || isa(lhs, TRef))

	# setup rhs

	(rhs_init, rhs_siz, rhs_ty, rhs_final, rhs_info) = setup_rhs(ctx, rhs)

	# setup lhs

	if isa(lhs, TVar)
		siz = gensym("siz")
		ty = gensym("ty")

		init_lhs = code_block(
			assignment(siz, rhs_siz),
			assignment(ty, rhs_ty),
			assignment(lhs.name, fun_call(:Array, ty, siz))
		)

		lhs_pre = init_lhs
		lhs_info = nothing
	else
		(lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
	end

	len = gensym("len")
	get_len = assignment(len, length_getter(ctx, lhs, lhs_info))

	# kernel & main loop

	i = gensym("i")
	kernel = compose_kernel(ctx, ex, lhs_info, rhs_info, i)
	
	main_loop = for_statement(i, 1, len, kernel)

	# integrate

	flatten_code_block(
		rhs_init,
		lhs_pre,
		get_len, 
		main_loop, 
		rhs_final)
end


# Element-wise 2D

function compile(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign)

	lhs = ex.lhs
	rhs = ex.rhs

	@assert(isa(lhs, TVar) || isa(lhs, TRef))

	# setup rhs

	(rhs_init, rhs_siz, rhs_ty, rhs_final, rhs_info) = setup_rhs(ctx, rhs)

	# setup lhs

	if isa(lhs, TVar)
		ty = gensym("ty")
		siz = gensym("siz")
		init_lhs = code_block(
			assignment(siz, rhs_siz),
			assignment(ty, rhs_ty),
			assignment(lhs.name, fun_call(:Array, ty, siz))
		)

		lhs_pre = init_lhs
		lhs_info = nothing
	else
		(lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
	end

	m = gensym("m")
	n = gensym("n")
	get_mn = expr(:(=), :( ($m, $n) ), size2d_getter(ctx, lhs, lhs_info))

	# kernel & main loop

	i = gensym("i")
	j = gensym("j")
	kernel = compose_kernel(ctx, ex, lhs_info, rhs_info, i, j)
	
	main_loop = for_statement(j, 1, n, for_statement(i, 1, m, kernel))

	# integrate

	flatten_code_block(
		rhs_init,
		lhs_pre,
		get_mn, 
		main_loop, 
		rhs_final)
end



# Full reduction

# function compose_reduc_core(ctx::ScalarContext, mode::EWiseMode, r::Union(TReduc, TColwiseReduc, TRowwiseReduc),  
# 	ty::Symbol, s::Symbol, n::Symbol, idxinfo...)

# 	@gensym x1 x2

# 	na = length(r.args)
# 	tfun = TFun{r.fun}()

# 	if na == 1
# 		arg1_pre, arg1_calc = compose(ctx, mode, r.args[1], idxinfo...)
# 		rempty, rinit, rupdate, rpost = reduc_computation(tfun, ty, s, n, x1)
# 		arg_pre = arg1_pre
# 		arg_calc = assignment(x1, arg1_calc)
# 	elseif na == 2
# 		arg1_pre, arg1_calc = compose(ctx, mode, r.args[1], idxinfo...)
# 		arg2_pre, arg2_calc = compose(ctx, mode, r.args[2], idxinfo...)
# 		rempty, rinit, rupdate, rpost = reduc_computation(tfun, ty, s, n, x1, x2)
# 		arg_pre = code_block(arg1_pre, arg2_pre)
# 		arg_calc = code_block(
# 			assignment(x1, arg1_calc),
# 			assignment(x2, arg2_calc)
# 		)
# 	end

# 	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost)
# end


# function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{1}, l::TExpr, r::TReduc, info)
# 	@gensym n i
# 	s, siz, ty = info

# 	# computation kernels

# 	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
# 		ctx, mode, r, ty, s, n, i)

# 	# store back statement

# 	if isa(l, TSym)
# 		store_back = quote end
# 	else
# 		store_back = :( $(compose(ctx, ScalarMode(), l)) = ($s) ) 
# 	end

# 	flatten_code_block(
# 		assignment(n, fun_call(qname(:to_length), siz)),
# 		arg_pre,
# 		assignment(s, rempty),
# 		for_statement( i, 1, n, code_block(
# 			arg_calc,
# 			rupdate
# 		) ),
# 		rpost,
# 		store_back
# 	)
# end


# function compose_reduc_main(ctx::ScalarContext, mode::EWiseMode{2}, l::TExpr, r::TReduc, info)
# 	@gensym len m n i j
# 	s, siz, ty = info

# 	# computation kernels

# 	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
# 		ctx, mode, r, ty, s, len, i, j)

# 	# store back statement

# 	if isa(l, TSym)
# 		store_back = quote end
# 	else
# 		store_back = :( $(compose(ctx, ScalarMode(), l)) = ($s) ) 
# 	end

# 	# major block (for where n > 0)

# 	mblock = flatten_code_block(
# 		arg_pre,
# 		assignment(s, rempty),
# 		for_statement( j, 1, n, code_block(
# 			for_statement( i, 1, m, code_block(
# 				arg_calc,
# 				rupdate
# 			) )
# 		) ),
# 		rpost
# 	)

# 	# wrap up 

# 	flatten_code_block(
# 		if_statement( :(length($siz) == 1), 
# 			code_block(
# 				assignment(m, :(($siz)[1]) ),
# 				assignment(n, :(1) ),
# 				assignment(len, m),
# 			),
# 			code_block(
# 				assignment(m, :(($siz)[1]) ),
# 				assignment(n, :(($siz)[2]) ),
# 				assignment(len, fun_call(:*, m, n)),
# 			)
# 		),
# 		if_statement( :( ($len) > 0 ),
# 			mblock,
# 			assignment(s, rempty)
# 		),
# 		store_back
# 	)
# end


# function compose_main(ctx::ScalarContext, mode::ReducMode, ex::TAssign, info)
# 	lhs = ex.lhs
# 	rhs = ex.rhs

# 	rmode = rhs.arg_mode
# 	if isa(rmode, EWiseMode{0})
# 		rmode = EWiseMode{1}()
# 	end

# 	main = compose_reduc_main(ctx, rmode, lhs, rhs, info)
# end


# function compose_main(ctx::ScalarContext, mode::ColwiseReducMode, ex::TAssign, info)
# 	lhs = ex.lhs
# 	rhs = ex.rhs
# 	@assert length(rhs.args) == 1

# 	siz, ty = info
# 	@gensym m n i j s

# 	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
# 		ctx, EWiseMode{2}(), rhs, ty, s, m, i, j)
# 	lhs_j_pre, lhs_j = compose_lhs(ctx, EWiseMode{1}(), lhs, j)

# 	flatten_code_block( 
# 		assignment(m, :(($siz)[1])),
# 		assignment(n, :(($siz)[2])),
# 		lhs_j_pre,
# 		arg_pre,
# 		for_statement( j, 1, n, flatten_code_block(
# 			assignment(s, rempty),
# 			for_statement( i, 1, m, code_block(
# 				arg_calc,
# 				rupdate
# 			) ),
# 			rpost,
# 			:( ($lhs_j) = ($s) )
# 		) )
# 	)
# end


# function compose_main(ctx::ScalarContext, mode::RowwiseReducMode, ex::TAssign, info)
# 	lhs = ex.lhs
# 	rhs = ex.rhs
# 	@assert length(rhs.args) == 1

# 	siz, ty = info
# 	@gensym m n i j s

# 	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
# 		ctx, EWiseMode{2}(), rhs, ty, s, n, i, j)
# 	lhs_i_pre, lhs_i = compose_lhs(ctx, EWiseMode{1}(), lhs, i)

# 	rpost2 = nothing
# 	if rpost != nothing
# 		rpost2 = for_statement(i, 1, m, code_block(
# 			assignment(s, lhs_i),
# 			rpost,
# 			:(($lhs_i) = ($s))
# 		) )
# 	end

# 	flatten_code_block(  # row-wise
# 		assignment(m, :(($siz)[1])),
# 		assignment(n, :(($siz)[2])),
# 		lhs_i_pre,
# 		arg_pre,
# 		assignment(j, 1),
# 		for_statement( i, 1, m,  code_block(
# 			arg_calc,
# 			:( $(lhs_i) = ($rinit) )
# 		)),
# 		for_statement( j, 2, n, 
# 			for_statement(i, 1, m, code_block(
# 				arg_calc,
# 				assignment(s, lhs_i),
# 				rupdate,
# 				:(($lhs_i) = ($s))
# 			))
# 		),
# 		rpost2
# 	)

# end


# Fast full reduction

# function compile_fast_reduc(ctx::ScalarContext, iex::TExpr, ex::TAssign)
# 	if !(isa(ex.lhs, TSym) && isa(ex.rhs, TReduc))
# 		throw(DeError("Invalid expression for fast_reduc."))
# 	end

# 	@gensym siz
# 	ty = iex.host

# 	siz_infer, mode = if isa(iex, TRefScalar1)
# 		:( ($siz) = ($(iex.i),) ), EWiseMode{1}()
# 	elseif isa(iex, TRefScalar2)
# 		:( ($siz) = ($(iex.i), $(iex.j)) ), EWiseMode{2}()
# 	else
# 		throw(DeError("Invalid size and type spec"))
# 	end

# 	s = ex.lhs.e
# 	main = compose_reduc_main(ctx, mode, ex.lhs, ex.rhs, (s, siz, ty))
# 	flatten_code_block(siz_infer, main)
# end

# function compile_fast_reduc(ctx::ScalarContext, iex::Expr, ex::Expr)
# 	compile_fast_reduc(ctx, texpr(iex), texpr(ex))
# end


##########################################################################
#
# 	code-generating macros
#
##########################################################################

function dump_devec(ex::Expr)
	println(compile(ScalarContext(), ex))
end

macro devec(assign_ex) 
	esc(compile(ScalarContext(), assign_ex))
end

macro inspect_devec(assign_ex)
	let code__ = compile(ScalarContext(), assign_ex)
		println("$assign_ex ==>")
		println(code__)
		esc(code__)
	end
end

# macro fast_reduc(info_ex, assign_ex)
# 	esc(compile_fast_reduc(ScalarContext(), info_ex, assign_ex))
# end

# macro inspect_fast_reduc(info_ex, assign_ex)
# 	let code__ = compile_fast_reduc(ScalarContext(), info_ex, assign_ex)
# 		println("$assign_ex ==>")
# 		println(code__)
# 		esc(code__)
# 	end
# end



