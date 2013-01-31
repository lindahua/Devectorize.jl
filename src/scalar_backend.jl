
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
get{T<:Number}(r::DeConst{T}, ::Integer) = r.val
get{T<:Number}(r::DeConst{T}, ::Integer, ::Integer) = r.val

# vector reader

type DeArrReader{T<:Number}
	src::Array{T}
end
get{T<:Number}(r::DeArrReader{T}, i::Integer) = r.src[i]
get{T<:Number}(r::DeArrReader{T}, i::Integer, j::Integer) = r.src[i,j]

# functions to generate accessors

devec_reader{T<:Number}(v::T) = DeConst{T}(v)
devec_reader{T<:Number}(a::Array{T}) = DeArrReader{T}(a)


##########################################################################
#
# 	code generators for ewise expressions
#
##########################################################################


function compose_ewise(::ScalarContext, t::DeNumber, idx::Symbol)
	@gensym rv
	pre = :()
	kernel = :( $(t.val) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, t::DeTerminal, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = devec_reader($(t.sym)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::DeRef{(DeColon,)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = devec_reader($(ex.host)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::DeRef{(DeColon,DeInt)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = devec_reader($(ex.host)) )
	kernel = :( get($rd, $idx, $(ex.args[2].val)) )
	(pre, kernel)
end

function compose_ewise(::ScalarContext, ex::DeRef{(DeColon,DeTerminal)}, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = devec_reader($(ex.host)) )
	kernel = :( get($rd, $idx, $(ex.args[2].sym)) )
	(pre, kernel)
end


function compose_ewise_lhs(::ScalarContext, lhs::DeTerminal, idx::Symbol)
	(	:( length($(lhs.sym)) ), 
		:( $(lhs.sym)[$(idx)] ) 
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::DeRef{(DeColon,)}, idx::Symbol)
	(	:( length($(lhs.host)) ),
		:( $(lhs.host)[$(idx)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::DeRef{(DeColon,DeInt)}, idx::Symbol)
	(	:( size($(lhs.host),1) ),
		:( $(lhs.host)[$(idx),$(lhs.args[2].val)] )
	)
end

function compose_ewise_lhs(::ScalarContext, lhs::DeRef{(DeColon,DeTerminal)}, idx::Symbol)
	(	:( size($(lhs.host),1) ),
		:( $(lhs.host)[$(idx),$(lhs.args[2].sym)] )
	)
end

function de_compile_ewise_1d(ctx::ScalarContext, lhs::AbstractDeExpr, rhs::AbstractDeExpr)
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

function de_compile_ewise(ctx::ScalarContext, lhs::DeTerminal, rhs::AbstractDeExpr) 

	ty_infer = gen_type_inference(rhs)
	size_infer = gen_size_inference(rhs)
	main_loop = de_compile_ewise_1d(ctx, lhs, rhs)
	
	# compose the whole thing
	
	:(
		($(lhs.sym)) = Array(($ty_infer), ($size_infer));
		($main_loop)
	)

end

de_compile_ewise(ctx::ScalarContext, lhs::DeRef{(DeColon,)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::DeRef{(DeColon,DeInt)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)

de_compile_ewise(ctx::ScalarContext, lhs::DeRef{(DeColon,DeTerminal)}, 
	rhs) = de_compile_ewise_1d(ctx, lhs, rhs)




##########################################################################
#
# 	code generators for reduction expressions
#
##########################################################################

# initializers

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:sum,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = zero($ty) )

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:max,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = typemin($ty) )

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:min,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = typemax($ty) )

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:mean,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = zero($ty) )


# updating kernels

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:sum,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) += ($x) )

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:max,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) = max(($dst), ($x)) )

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:min,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) = min(($dst), ($x)) )

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:mean,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) += ($x) )


# post-processers

compose_reduc_post{F,Args<:(AbstractDeExpr...,)}(ctx::ScalarContext, 
	rhs::DeCall{F,Args}, dst::Symbol, n::Symbol) = :( )

compose_reduc_post{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:mean,(A,)}, dst::Symbol, n::Symbol) = :( ($dst) /= ($n) )


# integrated

function de_compile_fullreduc{F,A<:AbstractDeExpr}(ctx::ScalarContext, 
	lhs::DeTerminal, rhs::DeCall{F,(A,)})

	# code for setup

	dst = lhs.sym
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


de_compile_reduc(ctx::ScalarContext, 
	lhs::DeTerminal,
	rhs::DeCall) = de_compile_fullreduc(ctx, lhs, rhs)



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

