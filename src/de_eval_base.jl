
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
	sym::Symbol
end

pretty(t::DeTerminal) = string(t.sym)

# generic delayed expression that may incorporate arbitrary number of args

type DeFunExpr{F, Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	args::Args
end

fsym{F,Args}(::DeFunExpr{F,Args}) = F

# a convenient function to create DeFunExpr, so that people do not have 
# to specify the type parameters of DeFunExpr

function de_expr{Args<:(AbstractDeExpr...,)}(f::Symbol, args::Args)
	return DeFunExpr{f,Args}(args)
end

# generate a pretty string
function pretty(ex::DeFunExpr)
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

_supported_ewise_funset = Set{(Symbol,Int)}()

macro def_uniop_result(s)
	eval( :( 
		result_type(::TFun{$s}, T::Type) = T;
		add!(_supported_ewise_funset, ($s, 1))
	) )
end

macro def_binop_result(s)
	eval( :( 
		result_type(::TFun{$s}, T1::Type, T2::Type) = promote_type(T1, T2); 
		add!(_supported_ewise_funset, ($s, 2))
	) )
end

macro def_triop_result(s)
	eval( :( 
		result_type(::TFun{$s}, T1::Type, T2::Type, T3::Type) = promote_type(T1, T2, T3);
		add!(_supported_ewise_funset, ($s, 3))
	) )
end

is_supported_ewise_fun(s::Symbol, nargs::Integer) = has(_supported_ewise_funset, (s, nargs))

# arithmetic operators

@def_uniop_result :+
@def_uniop_result :-

@def_binop_result :+
@def_binop_result :-
@def_binop_result :.+
@def_binop_result :.-
@def_binop_result :.*
@def_binop_result :./

@def_triop_result :+

@def_binop_result :min
@def_binop_result :max
@def_triop_result :clamp

# elementary math functions

@def_uniop_result :square
@def_uniop_result :inv
@def_uniop_result :sqrt
@def_uniop_result :cbrt

@def_binop_result :mod
@def_binop_result :pow
@def_binop_result :hypot

@def_uniop_result :round
@def_uniop_result :trunc
@def_uniop_result :ceil
@def_uniop_result :floor

@def_uniop_result :exp
@def_uniop_result :log
@def_uniop_result :exp2
@def_uniop_result :log2
@def_uniop_result :log10
@def_uniop_result :expm1
@def_uniop_result :log1p

@def_uniop_result :sin
@def_uniop_result :cos
@def_uniop_result :tan
@def_uniop_result :asin
@def_uniop_result :acos
@def_uniop_result :atan
@def_binop_result :atan2

@def_uniop_result :sinh
@def_uniop_result :cosh
@def_uniop_result :tanh
@def_uniop_result :qsinh
@def_uniop_result :qcosh
@def_uniop_result :qtanh

# special functions

@def_uniop_result :gamma
@def_uniop_result :lgamma
@def_uniop_result :digamma

@def_uniop_result :erf
@def_uniop_result :erfc



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
# 	test the type of an expression
#
##########################################################################

abstract DeExprKind

type ElementWiseMap <: DeExprKind end
type FullReduce <: DeExprKind end
type PartialReduce <: DeExprKind end	

de_expr_kind(ex::AbstractDeExpr) = ElementWiseMap()

# special cases

de_expr_kind{A<:AbstractDeExpr}(::DeFunExpr{:sum, A}) = FullReduce()
de_expr_kind{A<:AbstractDeExpr}(::DeFunExpr{:max, A}) = FullReduce()
de_expr_kind{A<:AbstractDeExpr}(::DeFunExpr{:min, A}) = FullReduce()


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



