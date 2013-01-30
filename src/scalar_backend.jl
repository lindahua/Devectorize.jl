
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
	
	if !is_supported_ewise_fun(F, 1)
		error("[de_generate]: $F with one argument is not a supported ewise function.")
	end
	
	@gensym rd1
	
	a1_pre, a1_kernel = devec_generate_rhs(ex.args[1], idx)
	pre = a1_pre
	kernel = :( ($F)( $a1_kernel ) )
	(pre, kernel)
end

function devec_generate_rhs{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeCall{F,(A1,A2)}, idx::Symbol)
	
	if !is_supported_ewise_fun(F, 2)
		error("[de_generate]: $F with two arguments is not a supported ewise function.")
	end
	
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
	
	if !is_supported_ewise_fun(F, 3)
		error("[de_generate]: $F with three arguments is not a supported ewise function.")
	end
	
	@gensym rd1
	
	a1_pre, a1_kernel = devec_generate_rhs(ex.args[1], idx)
	a2_pre, a2_kernel = devec_generate_rhs(ex.args[2], idx)
	a3_pre, a3_kernel = devec_generate_rhs(ex.args[3], idx)
	
	pre = :( $a1_pre, $a2_pre, $a3_pre )
	kernel = :( ($F)( $a1_kernel, $a2_kernel, $a3_kernel ) )
	(pre, kernel)
end


function devec_generate_ewise_core(lhs::Symbol, rhs::AbstractDeExpr)
	@gensym i
	rhs_pre, rhs_kernel = devec_generate_rhs(rhs, i)

	quote
		local n = length(($lhs))
		$rhs_pre
		for ($i) = 1 : n
			($lhs)[($i)] = ($rhs_kernel)
		end
	end
end


function devec_generate_ewise(lhs::Symbol, rhs::AbstractDeExpr)
	ty_infer = gen_type_inference(rhs.args[1])
	size_infer = gen_size_inference(rhs.args[1])
	core_loop = devec_generate_ewise_core(lhs, rhs)
	
	# compose the whole thing
	
	:(
		($lhs) = Array(($ty_infer), ($size_infer));
		($core_loop)
	)
end


function devec_generate_fullreduc{F,A<:AbstractDeExpr}(lhs::Symbol, rhs::DeCall{F,(A,)})
	@gensym i tmp
	ty_infer = gen_type_inference(rhs.args[1])
	siz_infer = gen_size_inference(rhs.args[1])
	rhs_pre, rhs_kernel = devec_generate_rhs(rhs.args[1], i)
	
	# generate reduction-specific part of codes
	
	if F == (:sum)	
		init = :( ($lhs) = zero($ty_infer) )
		kernel = :( ($lhs) += ($rhs_kernel) )
	elseif F == (:max)
		init = :( ($lhs) = typemin($ty_infer) )
		kernel = :( 
			let ($tmp) = ($rhs_kernel)
				if ($lhs) < ($tmp)
					($lhs) = ($tmp)
				end
			end
		)
	elseif F == (:min)
		init = :( ($lhs) = typemax($ty_infer) )
		kernel = :( 
			let ($tmp) = ($rhs_kernel)
				if ($lhs) > ($tmp)
					($lhs) = ($tmp)
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

function de_generate(::ScalarContext, assign_ex::Expr)
	# generate codes for cases where lhs is pre-allocated in correct size and type
	
	if !(assign_ex.head == :(=))
		error("[de_generate]: only supports assignment expression (at top level)")
	end
	
	lhs = assign_ex.args[1]
	rhs = de_wrap(assign_ex.args[2])
	
	if (isa(lhs, Symbol))
		
		if isa(rhs, DeCall)
			nargs = length(rhs.args)
			if is_supported_reduc_fun(fsym(rhs), nargs)
				devec_generate_fullreduc(lhs, rhs)
			else
				devec_generate_ewise(lhs, rhs)
			end
		else
			devec_generate_ewise(lhs, rhs)
		end
		
	elseif lhs.head == (:ref)
		
		la1 = lhs.args[1]
		la2 = lhs.args[2]
		
		if isa(la1, Symbol) && isa(la2, Symbol) && la2 == :(:)
			devec_generate_ewise_core(la1, rhs)
		else
			error("[de_generate]: the form of lhs is unsupported")
		end
		
	else
		error("[de_generate]: the form of lhs is unsupported")
	end
end


##########################################################################
#
# 	code-generating macros
#
##########################################################################

macro devec(assign_ex) 
	esc(begin 
		de_generate(ScalarContext(), assign_ex)
	end)
end


