
type ScalarContext <: CPUContext  # de-vectorized scalar for-loop
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

type DeCol{T<:Number}
	src::Array{T}
	icol::Int
end
get{T<:Number}(r::DeCol{T}, i::Int) = r.src[i, r.icol]

type DeRow{T<:Number}
	src::Array{T}
	irow::Int
end
get{T<:Number}(r::DeRow{T}, i::Int) = r.src[r.irow, i]


# functions to generate accessors

de_arr{T<:Number}(v::T) = DeConst{T}(v)
de_arr{T<:Number}(a::Array{T}) = DeArr{T}(a)
de_col{T<:Number}(a::Array{T}, i::Integer) = DeCol{T}(a, i)
de_row{T<:Number}(a::Array{T}, i::Integer) = DeRow{T}(a, i)

##########################################################################
#
# 	code generators for ewise expressions
#
##########################################################################

# right-hand-side code

function compose_ewise(::ScalarContext, t::TNum, idx::Symbol)
	@gensym rv
	pre = :()
	kernel = :( $(t.e) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, t::TSym, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_arr($(t.e)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TColon,)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_arr($(ex.host)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TColon,TInt)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_col($(ex.host), $(ex.args[2].e)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TColon,TSym)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_col($(ex.host), $(ex.args[2].e)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TInt,TColon)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_row($(ex.host), $(ex.args[1].e)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TSym, TColon)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = de_row($(ex.host), $(ex.args[1].e)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

# right-hand-side code for 2D

function compose_ewise(::ScalarContext, t::TNum, i::Symbol, j::Symbol)
	@gensym rv
	pre = :()
	kernel = :( $(t.e) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, t::TSym, i::Symbol, j::Symbol)
	@gensym rd
	pre = :( ($rd) = de_arr($(t.e)) )
	kernel = :( get($rd, $i, $j) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::TRef{(TColon,TColon)}, i::Symbol, j::Symbol)
	@gensym rd
	pre = :( ($rd) = de_arr($(t.host)) )
	kernel = :( get($rd, $i, $j) )
	(pre, kernel)
end


# left-hand-side code

function compose_ewise_lhs(::ScalarContext, lhs::TSym, idx::Symbol)
	(	:( length($(lhs.e)) ), 
		:( $(lhs.e)[$(idx)] ) 
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::TRef{(TColon,)}, idx::Symbol)
	(	:( length($(lhs.host)) ),
		:( $(lhs.host)[$(idx)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::TRef{(TColon,TInt)}, idx::Symbol)
	(	:( size($(lhs.host),1) ),
		:( $(lhs.host)[$(idx),$(lhs.args[2].e)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::TRef{(TColon,TSym)}, idx::Symbol)
	(	:( size($(lhs.host),1) ),
		:( $(lhs.host)[$(idx),$(lhs.args[2].e)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::TRef{(TInt,TColon)}, idx::Symbol)
	(	:( size($(lhs.host),2) ),
		:( $(lhs.host)[$(lhs.args[1].e), $(idx)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::TRef{(TSym,TColon)}, idx::Symbol)
	(	:( size($(lhs.host),2) ),
		:( $(lhs.host)[$(lhs.args[1].e), $(idx)] )
	)
end



function de_compile_ewise_1d(ctx::ScalarContext, lhs::TExpr, rhs::TExpr)
	@gensym i n
	lhs_len, lhs_expr = compose_ewise_lhs(ctx, lhs, i)
	rhs_pre, rhs_kernel = compose_ewise(ctx, rhs, i)

	# compose the main loop

	quote
		local ($n) = ($lhs_len)
		$rhs_pre
		for ($i) = 1 : ($n)
			$(lhs_expr) = ($rhs_kernel)
		end
	end
end

function de_compile_ewise(ctx::ScalarContext, lhs::TSym, rhs::TExpr) 

	ty_infer = gen_type_inference(rhs)
	size_infer = gen_size_inference(rhs)
	main_loop = de_compile_ewise_1d(ctx, lhs, rhs)
	
	# compose the whole thing
	
	:(
		($(lhs.e)) = Array(($ty_infer), ($size_infer));
		($main_loop)
	)

end

de_compile_ewise(ctx::ScalarContext, lhs::TRef{(TColon,)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::TRef{(TColon,TInt)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::TRef{(TColon,TSym)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::TRef{(TInt,TColon)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::TRef{(TSym,TColon)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)


##########################################################################
#
# 	code generators for reduction expressions
#
##########################################################################

# initializers

function compose_reduc_init(ctx::ScalarContext, rhs::TCall, dst::Symbol, ty::Symbol)
	f = rhs.fun
	f == (:sum) ?  :( ($dst) = zero($ty) ) :
	f == (:max) ?  :( ($dst) = typemin($ty) ) :
	f == (:min) ?  :( ($dst) = typemax($ty) ) :
	f == (:mean) ? :( ($dst) = zero($ty) ) :
	:()
end

function compose_reduc_kernel(ctx::ScalarContext, rhs::TCall, dst::Symbol, x::Symbol)
	f = rhs.fun
	f == (:sum) ?  :( ($dst) += ($x) ) :
	f == (:max) ?  :( ($dst) = max(($dst), ($x)) ) :
	f == (:min) ?  :( ($dst) = min(($dst), ($x)) ) :
	f == (:mean) ? :( ($dst) += ($x) ) :
	:()
end

function compose_reduc_post(ctx::ScalarContext, rhs::TCall, dst::Symbol, n::Symbol)
	f = rhs.fun
	f == (:mean) ? :( ($dst) /= ($n) ) :
	:()
end

# integrated

function de_compile_fullreduc(ctx::ScalarContext, lhs::TSym, rhs::TCall)

	# code for setup

	dst = lhs.e
	ty_infer = gen_type_inference(rhs)
	siz_infer = gen_size_inference(rhs)
	
	# generate reduction-specific part of codes
	
	@gensym ty i x n siz

	rhs_pre, rhs_kernel = compose_ewise(ctx, rhs.args[1], i)
	init = compose_reduc_init(ctx, rhs, dst, ty)
	kernel = compose_reduc_kernel(ctx, rhs, dst, x)
	post = compose_reduc_post(ctx, rhs, dst, n)
	
	# compose the whole thing
	
	:( 	($ty) = ($ty_infer);
		($init);
		($n) = 0;
		let ($siz) = ($siz_infer)
			($n) = prod(($siz))
			($rhs_pre)
			for ($i) = 1 : ($n)
				($x) = ($rhs_kernel)
				($kernel)
			end
		end;
		($post)
	)
	
end


de_compile_reduc(ctx::ScalarContext, lhs::TSym,
	rhs::TCall) = de_compile_fullreduc(ctx, lhs, rhs)


##########################################################################
#
# 	code-generating macros
#
##########################################################################

macro devec(assign_ex) 
	esc(begin 
		de_compile(ScalarContext(), assign_ex)
	end)
end

# macro to inspect the generated code

macro inspect_devec(assign_ex)
	begin
		code = de_compile(ScalarContext(), assign_ex)
		println(code)
	end
end

