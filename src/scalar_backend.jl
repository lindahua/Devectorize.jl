
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

get_value{T<:Number}(r::AbstractArray{T}, i::Int) = r[i]
get_value{T<:Number}(r::AbstractArray{T}, i::Int, j::Int) = r[i, j]



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

# assignment

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign, i::Symbol)
	lhs_pre, lhs_kernel = compose_lhs(ctx, mode, ex.lhs, i)
	rhs_pre, rhs_kernel = compose(ctx, mode, ex.rhs, i)

	pre = code_block(lhs_pre, rhs_pre)
	kernel = assignment(lhs_kernel, rhs_kernel)
	(pre, kernel)
end

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

function compose(ctx::ScalarContext, mode::EWiseMode{1}, ex::TSym, i::Symbol)
	pre = nothing
	kernel = fun_call(qname(:get_value), ex.e, i)
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

# assignment

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign, i::Symbol, j::Symbol)
	lhs_pre, lhs_kernel = compose_lhs(ctx, mode, ex.lhs, i, j)
	rhs_pre, rhs_kernel = compose(ctx, mode, ex.rhs, i, j)

	pre = code_block(lhs_pre, rhs_pre)
	kernel = assignment(lhs_kernel, rhs_kernel)
	(pre, kernel)
end


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

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TSym, i::Symbol, j::Symbol)
	pre = nothing
	kernel = fun_call(qname(:get_value), ex.e, i, j)
	(pre, kernel)
end

function compose(ctx::ScalarContext, mode::EWiseMode{2}, ex::TRef2D, i::Symbol, j::Symbol)
	pre = nothing
	kernel = :( $(ex.host)[($i), ($j)] )
	(pre, kernel)
end


# Maps

function compose(ctx::ScalarContext, mode::EWiseMode, ex::TMap, sinfo...)
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
# 	init-part composition
#
##########################################################################

compose_init(ctx::ScalarContext, mode::ScalarMode, ex::TAssign) = (nothing, nothing)

function compose_init(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign)

	@gensym siz len ty

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


function compose_init(ctx::ScalarContext, mode::ColwiseReducMode, ex::TAssign)
	@gensym siz ty

	r = ex.rhs

	if isa(ex.lhs, TSym)
		code = quote
			($ty) = $(type_inference(ex))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
			$(ex.lhs.e) = Array(($ty), (1, ($siz)[2]))
		end
	else
		code = quote
			($ty) = $(type_inference(ex.lhs))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
		end
	end

	(code, (siz, ty))
end


function compose_init(ctx::ScalarContext, mode::RowwiseReducMode, ex::TAssign)
	@gensym siz ty

	r = ex.rhs

	if isa(ex.lhs, TSym)
		code = quote
			($ty) = $(type_inference(ex))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
			$(ex.lhs.e) = Array(($ty), (($siz)[1], 1))
		end
	else
		code = quote
			($ty) = $(type_inference(ex.lhs))
			($siz) = DeExpr.to_size2d($(args_size_inference(r.args)))
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

	i = gensym("i")
	(pre, kernel) = compose(ctx, mode, ex, i)
	
	main_loop = for_statement(i, 1, len, kernel)
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

	main = code_block(
		assignment(m, :(($siz)[1])),
		assignment(n, :(($siz)[2])),
		for_statement(j, 1, n, 
			for_statement(i, 1, m, kernel)
		)
	)

	code_block(pre, main_loop)
end

# Full reduction

function compose_reduc_core(ctx::ScalarContext, mode::EWiseMode, r::Union(TReduc, TColwiseReduc, TRowwiseReduc),  
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
		for_statement( i, 1, n, code_block(
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
		for_statement( j, 1, n, code_block(
			for_statement( i, 1, m, code_block(
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


function compose_main(ctx::ScalarContext, mode::ColwiseReducMode, ex::TAssign, info)
	lhs = ex.lhs
	rhs = ex.rhs
	@assert length(rhs.args) == 1

	siz, ty = info
	@gensym m n i j s

	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
		ctx, EWiseMode{2}(), rhs, ty, s, m, i, j)
	lhs_j_pre, lhs_j = compose_lhs(ctx, EWiseMode{1}(), lhs, j)

	flatten_code_block( 
		assignment(m, :(($siz)[1])),
		assignment(n, :(($siz)[2])),
		lhs_j_pre,
		arg_pre,
		for_statement( j, 1, n, flatten_code_block(
			assignment(s, rempty),
			for_statement( i, 1, m, code_block(
				arg_calc,
				rupdate
			) ),
			rpost,
			:( ($lhs_j) = ($s) )
		) )
	)
end


function compose_main(ctx::ScalarContext, mode::RowwiseReducMode, ex::TAssign, info)
	lhs = ex.lhs
	rhs = ex.rhs
	@assert length(rhs.args) == 1

	siz, ty = info
	@gensym m n i j s

	(arg_pre, arg_calc, rempty, rinit, rupdate, rpost) = compose_reduc_core(
		ctx, EWiseMode{2}(), rhs, ty, s, n, i, j)
	lhs_i_pre, lhs_i = compose_lhs(ctx, EWiseMode{1}(), lhs, i)

	rpost2 = nothing
	if rpost != nothing
		rpost2 = for_statement(i, 1, m, code_block(
			assignment(s, lhs_i),
			rpost,
			:(($lhs_i) = ($s))
		) )
	end

	flatten_code_block(  # row-wise
		assignment(m, :(($siz)[1])),
		assignment(n, :(($siz)[2])),
		lhs_i_pre,
		arg_pre,
		assignment(j, 1),
		for_statement( i, 1, m,  code_block(
			arg_calc,
			:( $(lhs_i) = ($rinit) )
		)),
		for_statement( j, 2, n, 
			for_statement(i, 1, m, code_block(
				arg_calc,
				assignment(s, lhs_i),
				rupdate,
				:(($lhs_i) = ($s))
			))
		),
		rpost2
	)

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
	esc(compile(ScalarContext(), assign_ex))
end

macro inspect_devec(assign_ex)
	let code__ = compile(ScalarContext(), assign_ex)
		println("$assign_ex ==>")
		println(code__)
		esc(code__)
	end
end

macro fast_reduc(info_ex, assign_ex)
	esc(compile_fast_reduc(ScalarContext(), info_ex, assign_ex))
end

macro inspect_fast_reduc(info_ex, assign_ex)
	let code__ = compile_fast_reduc(ScalarContext(), info_ex, assign_ex)
		println("$assign_ex ==>")
		println(code__)
		esc(code__)
	end
end


##########################################################################
#
# 	@devec_transform - a code-generating macro for associative types 
#
##########################################################################
#
#   Starting with an associative type `d`, the following assigns or
#   replaces key `a` with the result of `x + y` where `x` or `y` could
#   be keys in `d`.
#
#       @devec_transform d  a = x + y
#
#   This basically converts to the following:
#
#       var1 = has(d, :x) ? d[:x] : x
#       var2 = has(d, :y) ? d[:y] : y
#       @devec res = var1 + var2
#       d[:a] = res
#
#   It contains machinery to convert the symbol to a key type
#   appropriate for the associative type. For example, DataFrames have
#   string keys, so the symbol from the expression needs to be
#   converted to a string. Also of issue is 
#
#   The following forms are supported:
#
#       @devec_transform d  a = x + y  b = x + sum(y)
#
#       @devec_transform(d, a => x + y, b => x + sum(y))
#
# 
##########################################################################


# The following is like Base.has, but converts symbols to appropriate
# key types.
xhas(d, key) = has(d, key)
xhas{K<:String,V}(d::Associative{K,V}, key) = has(d, string(key))

# The appropriate key for the type 
bestkey(d, key) = key
bestkey{K<:String,V}(d::Associative{K,V}, key) = string(key)

#### The following will be needed in package DataFrames for support
## xhas(d::AbstractDataFrame, key::Symbol) = has(d, string(key))
## bestkey(d::AbstractDataFrame, key) = string(key)
## bestkey(d::NamedArray, key) = string(key)

# This replaces symbols with gensym'd versions and updates
# a lookup dictionary.
replace_syms(x, lookup::Associative) = x
function replace_syms(s::Symbol, lookup::Associative)
    if has(lookup, s)
        lookup[s]
    else
        res = gensym("var")
        lookup[s] = res
        res
    end
end
function replace_syms(e::Expr, lookup::Associative)
    if e.head == :(=>)
        e.head = :(=)
    end
    if e.head == :call
        Expr(e.head, length(e.args) <= 1 ? e.args : [e.args[1], map(x -> replace_syms(x, lookup), e.args[2:end])], e.typ)
    else
        Expr(e.head, isempty(e.args) ? e.args : map(x -> replace_syms(x, lookup), e.args), e.typ)
    end
end

quot(value) = expr(:quote, value)  # Toivo special

function devec_transform_helper(d, args...)
    var_lookup = Dict()
    lhs_lookup = Dict()
    body = Any[]
    for ex in args
        push!(body, compile(ScalarContext(), replace_syms(ex, var_lookup)))
        lhs_lookup[ex.args[1]] = true
    end
    # header
    header = Any[]
    for (s,v) in var_lookup
        push!(header, :($v = DeExpr.xhas(d, DeExpr.bestkey(d, $(quot(s)))) ? d[DeExpr.bestkey(d, $(quot(s)))] : isdefined($(DeExpr.quot(s))) ? $s : nothing))
    end
    # trailer
    trailer = Any[]
    for (s,v) in lhs_lookup
        push!(trailer, :(d[DeExpr.bestkey(d, $(DeExpr.quot(s)))] = $(var_lookup[s])))
    end
    push!(trailer, :(d))
    esc(Expr(:block, [header, body, trailer], Any))
end

macro devec_transform(df, args...)
    devec_transform_helper(df, args...)
end
