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

# direct context: directly evaluate the expressions in CPU
abstract DirectContext <: EvalContext

# offshore context: transfer the computation to a specific device (e.g. GPU)
abstract OffshoreContext <: EvalContext

##########################################################################
#
#  Top level compilation skeletons
#
##########################################################################

function compile(ctx::EvalContext, top_expr::Expr)
	# generate codes given an expression

	h = top_expr.head
	if h == :(=) || is_opassign(h) || h == :(block)
		te = texpr(top_expr)
		compile(ctx, te)
	elseif h == :(*=)
		throw(DeError("DeExpr does not support *=, please use .*= for element-wise multiplication."))
	elseif h == :(/=)
		throw(DeError("DeExpr does not support /=, please use ./= for element-wise division."))
	else
		throw(DeError("Top level expression must be either an assignment, op-assignment, or a block."))
	end
end


function add_deps_to_queue(q::Array{TExpr, 1}, ex::TExpr)
	if isa(ex, TFunCall) 
		for a in ex.args
			add_deps_to_queue(q, a)
		end
		if ex.deps != nothing && !isempty(ex.deps)
			for d in ex.deps
				add_deps_to_queue(q, d)
				push!(q, d)
			end
		end
	elseif isa(ex, TAssign)
		add_deps_to_queue(q, ex.rhs)
	end
end

function compile(ctx::EvalContext, top_expr::TAssign)
	dep_queue = TExpr[]
	add_deps_to_queue(dep_queue, top_expr)

	if isempty(dep_queue)
		lhs = top_expr.lhs
		rhs = top_expr.rhs

		if is_trivial_assignment(top_expr)
			code_block( assignment(ju_expr(lhs), ju_expr(rhs)) )

		elseif isa(lhs, TGeneralVar)
			# to ensure no alias between left and right hand side
			tmp = gensym("tmp")
			safe_expr = tassign(tvar(tmp), rhs)
			flatten_code_block(
				code_block(compile(ctx, tmode(safe_expr), safe_expr)),
				assignment(ju_expr(lhs), tmp) )

		else 
			compile(ctx, tmode(top_expr), top_expr)
		end	
	else
		push!(dep_queue, top_expr)
		codes = [compile(ctx, tmode(e), e) for e in dep_queue]
		code_block(codes...)
	end
end

#####################################################################
#
#  Note: the following function:  
#
#		compile(ctx, mode, assign_ex) 
#
#	should be provided by the back-end
#
#####################################################################


function compile(ctx::EvalContext, ex::TBlock)
	codes = [compile(ctx, s) for s in ex.stmts]
	code_block(codes...)
end

