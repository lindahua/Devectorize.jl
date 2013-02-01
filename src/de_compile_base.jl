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

# direct context: directly evaluate the context in CPU
abstract DirectContext <: EvalContext

# managed context: transfer the computation to a specific device (e.g. GPU)
abstract OffshoreContext <: EvalContext


##########################################################################
#
#	Expression analysis
#
#	analyze the expression and gather information which may be used
#   to guide the compilation process
#
##########################################################################

abstract TExprKind

type EWiseExpr <: TExprKind end
type ReducExpr <: TExprKind end
type PartialReducExpr <: TExprKind end


function resolve_call_kind(ex::TCall)
	if is_reduc_call(ex)
		ReducExpr
	elseif is_ewise_call(ex)
		EWiseExpr
	else
		na = length(ex.args)
		throw(DeError("Call $(ex.fun) with $(na) arguments is not supported in DeExpr"))
	end
end

function analyze_expr(top_expr::TAssign)

	lhs = top_expr.lhs
	rhs = top_expr.rhs

	rhs_kind = 
		isa(rhs, TNum) ? EWiseExpr :
		isa(rhs, TSym) ? EWiseExpr :
		isa(rhs, TRef) ? EWiseExpr :
		isa(rhs, TCall) ? resolve_call_kind(rhs) :
		nothing

	if rhs_kind == nothing
		throw(DeError("The current form of rhs is unsupported by DeExpr"))
	end

	if isa(lhs, TSym)
		require_lhs_init = true
	else
		if !(rhs_kind == EWiseExpr)
			throw(DeError("The rhs must be an ewise-expression when lhs is a ref-expression"))
		end
		require_lhs_init = false
	end

	return (rhs_kind, require_lhs_init)
end




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
	
	de_compile(ctx, texpr(top_expr))
end

function de_compile(ctx::EvalContext, top_expr::TAssign)

	rhs_kind, require_lhs_init = analyze_expr(top_expr)

	lhs = top_expr.lhs
	rhs = top_expr.rhs

	lhs_init = require_lhs_init ? compose_lhs_init(ctx, rhs_kind(), lhs, rhs) : :()
	main_loop = compose_main_loop(ctx, rhs_kind(), lhs, rhs)

	# compiled code

	quote
		($lhs_init)
		($main_loop)
	end
end


##########################################################################
#
#  Middle level code composition
#
##########################################################################


function compose{A1<:TExpr}(ctx::EvalContext, kind::EWiseExpr, ex::TCall{(A1,)}, sinfo...)
	
	check_is_ewise(ex)
		
	a1_pre, a1_kernel = compose(ctx, kind, ex.args[1], sinfo...)
	pre = a1_pre
	kernel = :( ($(ex.fun))( $a1_kernel ) )
	(pre, kernel)
end

function compose{A1<:TExpr,A2<:TExpr}(ctx::EvalContext, kind::EWiseExpr, ex::TCall{(A1,A2)}, sinfo...)
	
	check_is_ewise(ex)
	
	a1_pre, a1_kernel = compose(ctx, kind, ex.args[1], sinfo...)
	a2_pre, a2_kernel = compose(ctx, kind, ex.args[2], sinfo...)
	pre = :( $a1_pre, $a2_pre )
	kernel = :( ($(ex.fun))( $a1_kernel, $a2_kernel ) )
	(pre, kernel)
end

function compose{A1<:TExpr,A2<:TExpr,A3<:TExpr}(ctx::EvalContext, kind::EWiseExpr, ex::TCall{(A1,A2,A3)}, sinfo...)
	
	check_is_ewise(ex)
		
	a1_pre, a1_kernel = compose(ctx, kind, ex.args[1], sinfo...)
	a2_pre, a2_kernel = compose(ctx, kind, ex.args[2], sinfo...)
	a3_pre, a3_kernel = compose(ctx, kind, ex.args[3], sinfo...)
	
	pre = :( $a1_pre, $a2_pre, $a3_pre )
	kernel = :( ($(ex.fun))( $a1_kernel, $a2_kernel, $a3_kernel ) )
	(pre, kernel)
end








