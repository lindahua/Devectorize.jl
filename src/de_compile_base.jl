# Generic routines to support compilation

##########################################################################
#
#   Types to express evaluation contexts
#
# 	A context refers to a specific configuration of the back-end, 
#	which can be scalar code, SIMD, CUDA, etc, or even hybrid of them.
#
# 	I organize types into a hierarchy, which may simplify later
# 	implementation.
#
# 	Here, most contexts are empty types. In practice, it is ok to have
# 	some information contained in the context 
# 	(e.g. the capability version of CUDA may be useful for code-gen)
#
##########################################################################

abstract EvalContext

abstract CPUContext <: EvalContext
abstract GPUContext <: EvalContext


##########################################################################
#
#  Top level compilation skeletons
#
##########################################################################

function de_compile(ctx::EvalContext, top_expr::Expr)
	# generate codes for cases where lhs is pre-allocated in correct size and type
	
	if !(top_expr.head == :(=))
		throw(DeError("Top level expression must be an assignment"))
	end
	
	de_compile(ctx, de_wrap(top_expr))
end

function de_compile(ctx::EvalContext, top_expr::DeAssign)
	lhs = top_expr.lhs
	rhs = top_expr.rhs

	if isa(lhs, DeTerminal)
		
		if isa(rhs, DeCall)
			if is_reduc_call(rhs)
				de_compile_reduc(ctx, lhs, rhs)
			else
				de_compile_ewise(ctx, lhs, rhs)
			end
		else
			de_compile_ewise(ctx, lhs, rhs)
		end
		
	else
		@assert isa(lhs, DeRef)
		de_compile_ewise(ctx, lhs, rhs)
	end
end


##########################################################################
#
#  Middle level code composition
#
##########################################################################


function compose_ewise{F,
	A1<:AbstractDeExpr}(ctx::EvalContext, ex::DeCall{F,(A1,)}, sinfo...)
	
	check_is_ewise(ex)
		
	a1_pre, a1_kernel = compose_ewise(ctx, ex.args[1], sinfo...)
	pre = a1_pre
	kernel = :( ($F)( $a1_kernel ) )
	(pre, kernel)
end

function compose_ewise{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ctx::EvalContext, ex::DeCall{F,(A1,A2)}, sinfo...)
	
	check_is_ewise(ex)
	
	a1_pre, a1_kernel = compose_ewise(ctx, ex.args[1], sinfo...)
	a2_pre, a2_kernel = compose_ewise(ctx, ex.args[2], sinfo...)
	pre = :( $a1_pre, $a2_pre )
	kernel = :( ($F)( $a1_kernel, $a2_kernel ) )
	(pre, kernel)
end

function compose_ewise{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr,
	A3<:AbstractDeExpr}(ctx::EvalContext, ex::DeCall{F,(A1,A2,A3)}, sinfo...)
	
	check_is_ewise(ex)
		
	a1_pre, a1_kernel = compose_ewise(ctx, ex.args[1], sinfo...)
	a2_pre, a2_kernel = compose_ewise(ctx, ex.args[2], sinfo...)
	a3_pre, a3_kernel = compose_ewise(ctx, ex.args[3], sinfo...)
	
	pre = :( $a1_pre, $a2_pre, $a3_pre )
	kernel = :( ($F)( $a1_kernel, $a2_kernel, $a3_kernel ) )
	(pre, kernel)
end








