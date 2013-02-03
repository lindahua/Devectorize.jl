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
#  Top level compilation skeletons
#
##########################################################################

function compile(ctx::EvalContext, top_expr::Expr)
	# generate codes for cases where lhs is pre-allocated in correct size and type

	if top_expr.head == :(=)
		te = texpr(top_expr)
		compile(ctx, te.mode, te)

	elseif top_expr.head == (:block)
		codes = Any[]
		for e in top_expr.args
			if isa(e, LineNumberNode) || e.head == (:line)
				continue
			end
			push!(codes, compile(ctx, e))
		end
		code_block(codes...)

	else
		throw(DeError("Top level expression must be either an assignment or a block"))
	end
end


function compile(ctx::EvalContext, top_expr::TAssign)
	mode = tmode(top_expr)
	compile(ctx, mode, top_expr)
end

function compile(ctx::EvalContext, mode::TMode, ex::TAssign)

	# compose parts

	mode_ = isa(mode, EWiseMode{0}) ? EWiseMode{1}() : mode

	(init, info) = compose_init(ctx, mode_, ex)
	main_body = compose_main(ctx, mode_, ex, info)

	# integrate
	flatten_code_block(code_block(init, main_body))
end



