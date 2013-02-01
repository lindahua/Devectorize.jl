
# This file provides support for meta-programming
#
# Primarily, it provides function to wrap Julia Expr instances
# to typed expressions
#

##########################################################################
#
# 	Types to express delayed expressions
#
##########################################################################

# types

abstract TExpr

type TNum{T<:Number} <: TExpr
	e::T
end

typealias TInt TNum{Int}

type TSym <: TExpr
	e::Symbol
end

type TColon <: TExpr
end

type TRef{Args<:(TExpr...,)} <: TExpr
	host::Symbol
	args::Args
end

type TCall{Args<:(TExpr...,)} <: TExpr
	fun::Symbol
	args::Args
end

type TAssign{Lhs<:TExpr, Rhs<:TExpr} <: TExpr
	lhs::Lhs
	rhs::Rhs
end

# convenient functions

function tnumber{T<:Number}(x::T)
	TNum{T}(x)
end

function tcall{Args<:(TExpr...,)}(f::Symbol, args::Args)
	TCall{Args}(f, args)
end

function tref{Args<:(TExpr...,)}(h::Symbol, args::Args)
	TRef{Args}(h, args)	
end

function tassign{Lhs<:TExpr, Rhs<:TExpr}(lhs::Lhs, rhs::Rhs)
	TAssign{Lhs, Rhs}(lhs, rhs)
end


# pretty printing

pretty(t::TNum) = string(t.e)

pretty(t::TSym) = string(t.e)

function pretty(t::TRef)
	pargs = join(map(pretty, t.args), ", ")
	"$(t.host)($pargs)"
end

function pretty(t::TCall)
	pargs = join(map(pretty, t.args), ", ")
	"$(t.fun)($pargs)"
end


##########################################################################
#
# 	supporting facility
#
##########################################################################

type DeError <: Exception
	msg::ASCIIString
end

function create_code_block(stmts...)
	expr(:block, stmts...)
end

function create_fun_call(funsym, args...)
	expr(:call, funsym, args...)
end

function create_assignment(lhs, rhs)
	expr(:(=), lhs, rhs)
end


##########################################################################
#
# 	the kind of a call
#
##########################################################################

function is_ewise_call{Args<:(TExpr...,)}(ex::TCall{Args})
	N = length(ex.args)
	return isa(get_op_kind(TCallSig{ex.fun, N}()), EWiseOp)
end

function is_reduc_call{Args<:(TExpr...,)}(ex::TCall{Args})
	N = length(ex.args)
	return isa(get_op_kind(TCallSig{ex.fun, N}()), ReducOp)
end

function check_is_ewise(ex::TCall)
	s = ex.fun
	na = length(ex.args)
	if !is_ewise_call(ex)
		throw(DeError("[de_compile]: $s with $na argument(s) is not a supported ewise operation."))
	end
end

function check_is_reduc(ex::TCall)
	s = ex.fun
	na = length(ex.args)
	if !is_reduc_call(ex)
		throw(DeError("[de_compile]: $s with $na argument(s) is not a supported reduction."))
	end
end


##########################################################################
#
# 	texpr: functions to wrap Expr to AST
#
##########################################################################

texpr{T<:Number}(x::T) = TNum{T}(x)
texpr(s::Symbol) = TSym(s)

function check_simple_ref(c)
	if !c
		throw(DeError("non-simple ref expression is not supported."))
	end
end

wrap_ref_arg(a::Symbol) = (a == :(:) ? TColon() : TSym(a))
wrap_ref_arg(a::Int) = TNum{Int}(a)

is_supported_lhs(::TExpr) = false
is_supported_lhs(::TSym) = true
is_supported_lhs(::TRef{(TColon,)}) = true
is_supported_lhs(::TRef{(TColon, TInt)}) = true
is_supported_lhs(::TRef{(TColon, TSym)}) = true
is_supported_lhs(::TRef{(TInt, TColon)}) = true
is_supported_lhs(::TRef{(TSym, TColon)}) = true
is_supported_lhs(::TRef{(TColon, TColon)}) = true

function texpr(ex::Expr) 

	if ex.head == :(call)

		fsym = ex.args[1]
		if !isa(fsym, Symbol)
			throw(DeError("call-expressions with non-symbol function name: $fsym"))
		end
		
		tcall(fsym, map(texpr, tuple(ex.args[2:]...)))
		
	elseif ex.head == :(ref)

		na = length(ex.args)
		check_simple_ref(na == 2 || na == 3)

		hsym = ex.args[1]
		check_simple_ref(isa(hsym, Symbol))

		SymOrNum = Union(Symbol, Number)

		if na == 2
			a1 = ex.args[2]
			check_simple_ref(isa(a1, SymOrNum))

			w1 = wrap_ref_arg(a1)
			tref(hsym, (w1,))
		else
			a1 = ex.args[2]
			a2 = ex.args[3]

			check_simple_ref(isa(a1, SymOrNum) && isa(a2, SymOrNum))

			w1 = wrap_ref_arg(a1)
			w2 = wrap_ref_arg(a2)
			tref(hsym, (w1, w2))
		end

	elseif ex.head == :(=)

		@assert length(ex.args) == 2
		lhs = texpr(ex.args[1])
		rhs = texpr(ex.args[2])

		if !is_supported_lhs(lhs)
			throw(DeError("Left-hand-side in current form is unsupported in TExpr"))
		end

		tassign(lhs, rhs)

	else
		throw(DeError("Unrecognized expression: $ex"))
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

ewise_shape(s1, s2, s3, s4...) = promote_shape(ewise_shape(s1, s2), ewise_shape(s3, s4...))

gen_size_inference(ex::TNum) = :( () )
gen_size_inference(ex::TSym) = :( size($(ex.e)) )
gen_size_inference(ex::TAssign) = :( $(gen_size_inference(ex.rhs)) )

gen_size_inference(ex::TRef{(TColon,)}) = :( (length($(ex.host)),) )
gen_size_inference(ex::TRef{(TColon, TColon)}) = :( size($(ex.host)) )

# for reference

gen_size_inference(ex::TRef{(TColon,TInt)}) = :( (size($(ex.host),1),) )
gen_size_inference(ex::TRef{(TColon,TSym)}) = :( (size($(ex.host),1),) )
gen_size_inference(ex::TRef{(TInt, TColon)}) = :( (1, size($(ex.host),2)) )
gen_size_inference(ex::TRef{(TSym, TColon)}) = :( (1, size($(ex.host),2)) )

# for function call

gen_size_inference{A1<:TExpr}(ex::TCall{(A1,)}) = :( 
		$(gen_size_inference(ex.args[1])) 
)
	
function gen_size_inference(ex::TCall) 
	arg_stmts = [gen_size_inference(a) for a in ex.args]
	create_fun_call(:ewise_shape, arg_stmts...)
end



##########################################################################
#
# 	type inference
#
##########################################################################

gen_type_inference(rt::Symbol, ex::TNum) = :( ($rt) = typeof($(ex.e)) )
gen_type_inference(rt::Symbol, ex::TSym) = :( ($rt) = eltype($(ex.e)) )
gen_type_inference(rt::Symbol, ex::TAssign) = :( ($rt) = $(gen_type_inference(rt, ex.rhs)) )
gen_type_inference(rt::Symbol, ex::TRef) = :( ($rt) = eltype($(ex.host)) )


function gen_type_inference(rt::Symbol, ex::TCall) 

	tf = TFun{ex.fun}()

	na = length(ex.args)
	tas = [gensym("ta$i") for i = 1 : na]
	argty_stmts = [gen_type_inference(tas[i], ex.args[i]) for i = 1 : na]

	create_code_block( 
		argty_stmts...,
		create_assignment(rt, create_fun_call(:result_type, tf, tas...)) 
	)
end


