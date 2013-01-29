
##########################################################################
#
# 	Types to express delayed expressions
#
##########################################################################

abstract AbstractDeExpr

type DeNumber <: AbstractDeExpr
	val::Number
end

pretty(t::DeNumber) = string(t.val)

type DeTerminal <: AbstractDeExpr
	arg::Symbol
end

pretty(t::DeTerminal) = string(t.arg)

# generic delayed expression that may incorporate arbitrary number of args

type DeExpr{F, Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	args::Args
end

fsym{F,Args}(::DeExpr{F,Args}) = F

# a convenient function to create DeExpr, so that people do not have 
# to specify the type parameters of DeExpr

function de_expr{Args<:(AbstractDeExpr...,)}(f::Symbol, args::Args)
	return DeExpr{f,Args}(args)
end

# generate a pretty string
function pretty(ex::DeExpr)
	pargs = join(map(pretty, ex.args), ", ")
	"$(fsym(ex))($pargs)"
end


##########################################################################
#
# 	Result type inference
#
##########################################################################

type TFun{Sym} 
end

result_type(::TFun{:+}, T::Type) = T
result_type(::TFun{:-}, T::Type) = T

result_type(::TFun{:+}, T1::Type, T2::Type) = promote_type(T1, T2)
result_type(::TFun{:-}, T1::Type, T2::Type) = promote_type(T1, T2)


##########################################################################
#
# 	de_wrap: functions to wrap Expr to AST
#
##########################################################################

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

type ScalarContext <: CPUContext  # de-vectorized scalar for-loop
end

type SIMDContext <: CPUContext
end

type CUDAContext <: GPUContext
end

