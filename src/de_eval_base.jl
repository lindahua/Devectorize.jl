
##########################################################################
#
# 	Types to express delayed expressions
#
##########################################################################

# types

abstract AbstractDeExpr

type DeNumber <: AbstractDeExpr
	val::Number
end

type DeTerminal <: AbstractDeExpr
	sym::Symbol
end

type DeRef{Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	host::Symbol
	args::Args
end

type DeFunExpr{F, Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	args::Args
end

# convenient functions

function de_expr{Args<:(AbstractDeExpr...,)}(f::Symbol, args::Args)
	DeFunExpr{f,Args}(args)
end

function de_ref{Args<:(AbstractDeExpr...,)}(args::Args)
	DeRef{Args}(args)	
end

fsym{F,Args}(::DeFunExpr{F,Args}) = F

# pretty printing

pretty(t::DeNumber) = string(t.val)

pretty(t::DeTerminal) = string(t.sym)

function pretty(ex::DeRef)
	pargs = join(map(pretty, ex.args), ", ")
	"$(ex.host)($pargs)"
end

function pretty(ex::DeFunExpr)
	pargs = join(map(pretty, ex.args), ", ")
	"$(fsym(ex))($pargs)"
end


##########################################################################
#
# 	exception types
#
##########################################################################

type DeError <: Exception
	msg::ASCIIString
end


##########################################################################
#
# 	Supported operations & functions
#
##########################################################################

type TFun{Sym} 
end

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

_supported_ewise_funset = Set{(Symbol,Int)}()
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

# reduction functions

_supported_reduc_funset = Set{(Symbol,Int)}()
is_supported_reduc_fun(s::Symbol, nargs::Integer) = has(_supported_reduc_funset, (s, nargs))

macro def_reductor(s)
	eval( :( 
		result_type(::TFun{$s}, T::Type) = T;
		add!(_supported_reduc_funset, ($s, 1))
	) )
end

@def_reductor :sum
@def_reductor :max
@def_reductor :min
@def_reductor :mean
@def_reductor :normfro


##########################################################################
#
# 	de_wrap: functions to wrap Expr to AST
#
##########################################################################

de_wrap{T<:Number}(x::T) = DeNumber(x)
de_wrap(s::Symbol) = DeTerminal(s)

function de_wrap(ex::Expr) 
	if ex.head == :(call)
		fsym = ex.args[1]
		if !isa(fsym, Symbol)
			throw(DeError("call-expressions in DeExpr must make the function name explicit."))
		end
		
		de_expr(fsym, map(de_wrap, tuple(ex.args[2:]...)))
		
	elseif ex.head == :(ref)
		hsym = ex.args[1]
		if !isa(hsym, Symbol)
			throw(DeError("ref-expressions in DeExpr must make the host name explicit."))
		end
		
		de_ref(hsym, map(de_wrap, tuple(ex.args[2:]...)))
	end
end


##########################################################################
#
# 	shape inference
#
##########################################################################

ewise_shape(::(), ::()) = ()
ewise_shape(::(), s) = s
ewise_shape(s, ::()) = s
ewise_shape(s1, s2) = promote_shape(s1, s2)

ewise_shape(::(), ::(), ::()) = ()
ewise_shape(s1, ::(), ::()) = s2
ewise_shape(::(), s2, ::()) = s2
ewise_shape(::(), ::(), s3) = s3
ewise_shape(s1, s2, ::()) = promote_shape(s1, s2)
ewise_shape(s1, ::(), s3) = promote_shape(s1, s3)
ewise_shape(::(), s2, s3) = promote_shape(s2, s3)
ewise_shape(s1, s2, s3) = promote_shape(promote_shape(s1, s2), s3)

ewise_result_shape(
	a1::AbstractArray, 
	a2::AbstractArray, 
	a3::AbstractArray) = promote_shape( promote_shape(size(a1), size(a2)), size(a3) )

gen_size_inference(ex::DeNumber) = :( () )
gen_size_inference(ex::DeTerminal) = :( size($(ex.sym)) )

gen_size_inference{F,
	A1<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,)}) = :( 
		$(gen_size_inference(ex.args[1])) 
)
	
gen_size_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,A2)}) = :( 
		ewise_shape( 
			$(gen_size_inference(ex.args[1])), 
			$(gen_size_inference(ex.args[2])) ) 
)

gen_size_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr,
	A3<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,A2,A3)}) = :( 
		ewise_shape( 
			$(gen_size_inference(ex.args[1])), 
			$(gen_size_inference(ex.args[2])),
			$(gen_size_inference(ex.args[3])) ) 
)


##########################################################################
#
# 	type inference
#
##########################################################################

gen_type_inference(ex::DeNumber) = :( typeof($(ex.val)) )
gen_type_inference(ex::DeTerminal) = :( eltype($(ex.sym)) )

function gen_type_inference{F,
	A1<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,)})
	
		t = TFun{F}()
		:( result_type(
			$t,
			$(gen_type_inference(ex.args[1])) 
		) )
end

function gen_type_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,A2)}) 
	
		t = TFun{F}()
		:( result_type(
			$t,
			$(gen_type_inference(ex.args[1])),
			$(gen_type_inference(ex.args[2]))
		) )
end
	
function gen_type_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr,
	A3<:AbstractDeExpr}(ex::DeFunExpr{F,(A1,A2,A3)})
	
		t = TFun{F}()
		:( result_type(
			$t,
			$(gen_type_inference(ex.args[1])),
			$(gen_type_inference(ex.args[2])),
			$(gen_type_inference(ex.args[3])) 
		) )
end


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



