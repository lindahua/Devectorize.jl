
# types to express delayed expressions
#
# The type system here basically follow Krys, but I change
# the names to make it more consistent with Julia's
#

abstract AbstractDeExpr

type DeNumber <: AbstractDeExpr
	val::Number
end

type DeTerminal <: AbstractDeExpr
	arg::Symbol
end

# generic delayed expression that may incorporate arbitrary number of args

type DeExpr{F, Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	args::Args
end

# a convenient function to create DeExpr, so that people do not have 
# to specify the type parameters of DeExpr

function de_expr{Args<:(AbstractDeExpr...,)}(f::Symbol, args::Args)
	return DeExpr{f,Args}(args)
end


# functions to infer element-type of the result

result_type(::AddTag, T::Type) = T
result_type(::SubTag, T::Type) = T

result_type(::AddTag, T1::Type, T2::Type) = promote_type(T1, T2)
result_type(::SubTag, T1::Type, T2::Type) = promote_type(T1, T2)


# functions to wrap AST to delayed expressions

de_wrap{T<:Number}(x::T) = DeNumber(x)
de_wrap(s::Symbol) = DeTerminal(s)

# Any sane way to convert an array to a tuple ?
function array_to_tuple(a)
	n = length(a)
	n == 0 ? () :
	n == 1 ? (a[1],) :
	n == 2 ? (a[1], a[2]) :
	n == 3 ? (a[1], a[2], a[3]) :
	n == 4 ? (a[1], a[2], a[3], a[4]) :
	n == 5 ? (a[1], a[2], a[3], a[4], a[5]) :
	n == 6 ? (a[1], a[2], a[3], a[4], a[5], a[6]) :
	error("array_to_tuple cannot support length > 6")
end

de_wrap(ex::Expr) = de_expr(
	ex.args[1], 
	map(de_wrap, array_to_tuple(ex.args[2:]))
)


# types to express evaluation contexts
#
# A context refers to a specific configuration of the back-end, which
# can be scalar code, SIMD, CUDA, etc, or even hybrid of them.
#
# Again, I organize types into a hierarchy, which may simplify later
# implementation.
#
# Here, most contexts are empty types. In practice, it is ok to have
# some information contained in the context 
# (e.g. the capability version of CUDA may be useful for code-gen)
# 

abstract EvalContext

abstract CPUContext <: EvalContext
abstract GPUContext <: EvalContext

type ScalarContext <: CPUContext  # de-vectorized scalar for-loop
end

type SIMDContext <: CPUContext
end

type CUDAContext <: GPUContext
end

# context-aware code generation functions
#
# Krys used de_jl_eval, etc for this purpose.
#
# I think unifying the function name to de_generate,
# and then dispatch the actual implementation
# according to context might be a better approach.
# This fits well with Julia's multi-dispatch
# mechanism.
#
# Also, I think "generate" is more appropriate
# than "eval" here, as such functions is to 
# generate codes not to evaluate them
#

function de_generate(ctx::EvalContext, ex::Expr)
	# returns the generated code in an appropriate way
end

# Finally, it is good to have a macro system to simplify
# the coding.
#
# For normal users, they can simply rely on a default 
# evaluation policy. However, advanced users may choose
# a specific context to optimize their computation.
#






