
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


function devec_generate_rhs(t::DeNumber, idx::Symbol)
	@gensym rv
	pre = :()
	kernel = :( $(t.val) )
	(pre, kernel)
end

function devec_generate_rhs(t::DeTerminal, idx::Symbol)
	@gensym rd
	pre = :( ($rd) = devec_reader($(t.sym)) )
	kernel = :( get($rd, $idx) )
	(pre, kernel)
end

function devec_generate_rhs{F,
	A1<:AbstractDeExpr}(ex::DeCall{F,(A1,)}, idx::Symbol)
	
	check_is_ewise(ex)
	
	@gensym rd1
	
	a1_pre, a1_kernel = devec_generate_rhs(ex.args[1], idx)
	pre = a1_pre
	kernel = :( ($F)( $a1_kernel ) )
	(pre, kernel)
end

function devec_generate_rhs{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeCall{F,(A1,A2)}, idx::Symbol)
	
	check_is_ewise(ex)
	
	@gensym rd1
	
	a1_pre, a1_kernel = devec_generate_rhs(ex.args[1], idx)
	a2_pre, a2_kernel = devec_generate_rhs(ex.args[2], idx)
	pre = :( $a1_pre, $a2_pre )
	kernel = :( ($F)( $a1_kernel, $a2_kernel ) )
	(pre, kernel)
end

function devec_generate_rhs{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr,
	A3<:AbstractDeExpr}(ex::DeCall{F,(A1,A2,A3)}, idx::Symbol)
	
	check_is_ewise(ex)
	
	@gensym rd1
	
	a1_pre, a1_kernel = devec_generate_rhs(ex.args[1], idx)
	a2_pre, a2_kernel = devec_generate_rhs(ex.args[2], idx)
	a3_pre, a3_kernel = devec_generate_rhs(ex.args[3], idx)
	
	pre = :( $a1_pre, $a2_pre, $a3_pre )
	kernel = :( ($F)( $a1_kernel, $a2_kernel, $a3_kernel ) )
	(pre, kernel)
end


function devec_generate_ewise_core(dst::Symbol, rhs::AbstractDeExpr)
	@gensym i
	rhs_pre, rhs_kernel = devec_generate_rhs(rhs, i)

	quote
		local n = length(($dst))
		$rhs_pre
		for ($i) = 1 : n
			($dst)[($i)] = ($rhs_kernel)
		end
	end
end


function devec_generate_ewise(lhs::DeTerminal, rhs::AbstractDeExpr)

	dst = lhs.sym
	ty_infer = gen_type_inference(rhs.args[1])
	size_infer = gen_size_inference(rhs.args[1])
	core_loop = devec_generate_ewise_core(dst, rhs)
	
	# compose the whole thing
	
	:(
		($dst) = Array(($ty_infer), ($size_infer));
		($core_loop)
	)
end


function devec_generate_fullreduc{F,A<:AbstractDeExpr}(lhs::DeTerminal, rhs::DeCall{F,(A,)})
	@gensym i tmp
	dst = lhs.sym
	ty_infer = gen_type_inference(rhs.args[1])
	siz_infer = gen_size_inference(rhs.args[1])
	rhs_pre, rhs_kernel = devec_generate_rhs(rhs.args[1], i)
	
	# generate reduction-specific part of codes
	
	if F == (:sum)	
		init = :( ($dst) = zero($ty_infer) )
		kernel = :( ($dst) += ($rhs_kernel) )
	elseif F == (:max)
		init = :( ($dst) = typemin($ty_infer) )
		kernel = :( 
			let ($tmp) = ($rhs_kernel)
				if ($dst) < ($tmp)
					($dst) = ($tmp)
				end
			end
		)
	elseif F == (:min)
		init = :( ($dst) = typemax($ty_infer) )
		kernel = :( 
			let ($tmp) = ($rhs_kernel)
				if ($dst) > ($tmp)
					($dst) = ($tmp)
				end
			end
		)
	else
		error("Unsupported reduction function $fsym")
	end
	
	# compose the whole thing
	
	:( 	$init;
		let siz = ($siz_infer)
			local n = prod(siz)
			($rhs_pre)
			for ($i) = 1 : n
				($kernel)
			end
		end
	)
	
end

function de_compile(::ScalarContext, assign_ex::Expr)
	# generate codes for cases where lhs is pre-allocated in correct size and type
	
	if !(assign_ex.head == :(=))
		throw(DeError("Top level expression must be an assignment"))
	end
	
	lhs = de_wrap(assign_ex.args[1])
	rhs = de_wrap(assign_ex.args[2])

	if isa(lhs, DeTerminal)
		
		if isa(rhs, DeCall)
			nargs = length(rhs.args)
			if is_reduc_call(rhs)
				devec_generate_fullreduc(lhs, rhs)
			else
				devec_generate_ewise(lhs, rhs)
			end
		else
			devec_generate_ewise(lhs, rhs)
		end
		
	elseif isa(lhs, DeRef)
		
		if length(lhs.args) == 1 && lhs.args[1] == :(:)
			devec_generate_ewise_core(lhs.host, rhs)
		else
			throw(DeError("the form of left-hand-side is unsupported"))
		end
		
	else
		throw(DeError("the form of right-hand-side is unsupported"))
	end
end


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


