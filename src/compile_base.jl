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

	h = top_expr.head
	if h == :(=) || h == :(+=) || h == :(-=) || h == :(.*=) || h == :(./=) || h == :(block)
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

		if isa(lhs, TSym)
			if isa(rhs, TSym)
				# trivial assignment (no need to compile)
				code_block( assignment(lhs.e, rhs.e) )
			else
				# to ensure no alias between left and right hand side
				tmp = gensym("tmp")
				emode = top_expr.mode
				safe_expr = TAssign(TSym(tmp), rhs, emode)
				flatten_code_block(
					code_block(compile(ctx, emode, safe_expr)),
					assignment(lhs.e, tmp) )
			end
		else
			compile(ctx, top_expr.mode, top_expr)
		end		
	else
		push!(dep_queue, top_expr)
		codes = [compile(ctx, tmode(e), e) for e in dep_queue]
		code_block(codes...)
	end
end

function compile(ctx::EvalContext, mode::TMode, ex::TAssign)

	# compose parts

	mode_ = isa(mode, EWiseMode{0}) ? EWiseMode{1}() : mode

	(init, info) = compose_init(ctx, mode_, ex)
	main_body = compose_main(ctx, mode_, ex, info)

	# integrate
	flatten_code_block(code_block(init, main_body))
end


function compile(ctx::EvalContext, ex::TBlock)
	codes = [compile(ctx, s) for s in ex.stmts]
	code_block(codes...)
end

