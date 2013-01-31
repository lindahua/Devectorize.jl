
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
get{T<:Number}(r::DeConst{T}, i::Integer) = r.val

# vector reader

type DeVecReader{T<:Number}
	src::Vector{T}
end
get{T<:Number}(r::DeVecReader{T}, i::Integer) = r.src[i]


##########################################################################
#
# 	function to generate accessors
#
##########################################################################

devec_reader{T<:Number}(v::T) = DeConst{T}(v)
devec_reader{T<:Number}(a::Vector{T}) = DeVecReader{T}(a)


##########################################################################
#
# 	code generators
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


function de_compile_ewise_core(ctx::ScalarContext, dst::Symbol, rhs::AbstractDeExpr)
	@gensym i
	rhs_pre, rhs_kernel = compose_ewise(ctx, rhs, i)

	quote
		local n = length(($dst))
		$rhs_pre
		for ($i) = 1 : n
			($dst)[($i)] = ($rhs_kernel)
		end
	end
end



compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:sum,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = zero($ty) )

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:max,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = typemin($ty) )

compose_reduc_init{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:min,(A,)}, dst::Symbol, ty::Symbol) = :( ($dst) = typemax($ty) )


compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:sum,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) += ($x) )

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:max,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) = max(($dst), ($x)) )

compose_reduc_kernel{A<:AbstractDeExpr}(ctx::ScalarContext, 
	rhs::DeCall{:min,(A,)}, dst::Symbol, x::Symbol) = :( ($dst) = min(($dst), ($x)) )


function de_compile_fullreduc{F,A<:AbstractDeExpr}(ctx::ScalarContext, 
	lhs::DeTerminal, rhs::DeCall{F,(A,)})

	# code for setup

	dst = lhs.sym
	ty_infer = gen_type_inference(rhs.args[1])
	siz_infer = gen_size_inference(rhs.args[1])
	
	# generate reduction-specific part of codes
	
	@gensym ty i x

	rhs_pre, rhs_kernel = compose_ewise(ctx, rhs.args[1], i)
	init = compose_reduc_init(ctx, rhs, dst, ty)
	kernel = compose_reduc_kernel(ctx, rhs, dst, x)
	
	# compose the whole thing
	
	:( 	($ty) = ($ty_infer);
		($init);
		let siz = ($siz_infer)
			local n = prod(siz)
			($rhs_pre)
			for ($i) = 1 : n
				($x) = ($rhs_kernel)
				($kernel)
			end
		end
	)
	
end


# specialized decompile functions

de_compile_ewise(ctx::ScalarContext, 
	lhs::DeRef{(DeColon,)}, 
	rhs::AbstractDeExpr) = de_compile_ewise_core(ctx, lhs.host, rhs)

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


