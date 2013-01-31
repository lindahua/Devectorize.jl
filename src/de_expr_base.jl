
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

type DeColon <: AbstractDeExpr
end

type DeRef{Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	host::Symbol
	args::Args
end

type DeCall{F, Args<:(AbstractDeExpr...,)} <: AbstractDeExpr
	args::Args
end

type DeAssign{Lhs<:AbstractDeExpr, Rhs<:AbstractDeExpr} <: AbstractDeExpr
	lhs::Lhs
	rhs::Rhs
end

# convenient functions

function de_call{Args<:(AbstractDeExpr...,)}(f::Symbol, args::Args)
	DeCall{f,Args}(args)
end

function de_ref{Args<:(AbstractDeExpr...,)}(h::Symbol, args::Args)
	DeRef{Args}(h, args)	
end

function de_assign{Lhs<:AbstractDeExpr, Rhs<:AbstractDeExpr}(lhs::Lhs, rhs::Rhs)
	DeAssign{Lhs, Rhs}(lhs, rhs)
end


fsym{F,Args}(::DeCall{F,Args}) = F

# pretty printing

pretty(t::DeNumber) = string(t.val)

pretty(t::DeTerminal) = string(t.sym)

function pretty(ex::DeRef)
	pargs = join(map(pretty, ex.args), ", ")
	"$(ex.host)($pargs)"
end

function pretty(ex::DeCall)
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
# 	the kind of a call
#
##########################################################################

function is_ewise_call{F,Args<:(AbstractDeExpr...,)}(ex::DeCall{F,Args})
	N = length(ex.args)
	return isa(get_op_kind(TCall{F,N}()), EWiseOp)
end

function is_reduc_call{F,Args<:(AbstractDeExpr...,)}(ex::DeCall{F,Args})
	N = length(ex.args)
	return isa(get_op_kind(TCall{F,N}()), ReducOp)
end

function check_is_ewise(ex::DeCall)
	s = fsym(ex)
	na = length(ex.args)
	if !is_ewise_call(ex)
		throw(DeError("[de_compile]: $s with $na argument(s) is not a supported ewise operation."))
	end
end

function check_is_reduc(ex::DeCall)
	s = fsym(ex)
	na = length(ex.args)
	if !is_reduc_call(ex)
		throw(DeError("[de_compile]: $s with $na argument(s) is not a supported reduction."))
	end
end


##########################################################################
#
# 	de_wrap: functions to wrap Expr to AST
#
##########################################################################

de_wrap{T<:Number}(x::T) = DeNumber(x)
de_wrap(s::Symbol) = DeTerminal(s)

function check_simple_ref(c)
	if !c
		throw(DeError("non-simple ref expression is not supported."))
	end
end

wrap_ref_arg(a::Symbol) = (a == :(:) ? DeColon() : DeTerminal(a))

is_supported_lhs(::AbstractDeExpr) = false
is_supported_lhs(::DeTerminal) = true
is_supported_lhs(::DeRef{(DeColon,)}) = true

function de_wrap(ex::Expr) 

	if ex.head == :(call)

		fsym = ex.args[1]
		if !isa(fsym, Symbol)
			throw(DeError("call-expressions with non-symbol function name: $fsym"))
		end
		
		de_call(fsym, map(de_wrap, tuple(ex.args[2:]...)))
		
	elseif ex.head == :(ref)

		na = length(ex.args)
		check_simple_ref(na == 2 || na == 3)

		hsym = ex.args[1]
		if na == 2
			a1 = ex.args[2]
			check_simple_ref(isa(a1, Symbol))

			w1 = wrap_ref_arg(a1)
			de_ref(hsym, (w1,))
		else
			a1 = ex.args[2]
			a2 = ex.args[3]
			check_simple_ref(isa(a1, Symbol) && isa(a2, Symbol))

			w1 = wrap_ref_arg(a1)
			w2 = wrap_ref_arg(a2)
			de_ref(hsym, (w1, w2))
		end

	elseif ex.head == :(=)

		@assert length(ex.args) == 2
		lhs = de_wrap(ex.args[1])
		rhs = de_wrap(ex.args[2])

		if !is_supported_lhs(lhs)
			throw(DeError("Left-hand-side in current form is unsupported in DeExpr"))
		end

		de_assign(lhs, rhs)

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

ewise_result_shape(
	a1::AbstractArray, 
	a2::AbstractArray, 
	a3::AbstractArray) = promote_shape( promote_shape(size(a1), size(a2)), size(a3) )

gen_size_inference(ex::DeNumber) = :( () )
gen_size_inference(ex::DeTerminal) = :( size($(ex.sym)) )

gen_size_inference{F,
	A1<:AbstractDeExpr}(ex::DeCall{F,(A1,)}) = :( 
		$(gen_size_inference(ex.args[1])) 
)
	
gen_size_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeCall{F,(A1,A2)}) = :( 
		ewise_shape( 
			$(gen_size_inference(ex.args[1])), 
			$(gen_size_inference(ex.args[2])) ) 
)

gen_size_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr,
	A3<:AbstractDeExpr}(ex::DeCall{F,(A1,A2,A3)}) = :( 
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
	A1<:AbstractDeExpr}(ex::DeCall{F,(A1,)})
	
		t = TFun{F}()
		:( result_type(
			$t,
			$(gen_type_inference(ex.args[1])) 
		) )
end

function gen_type_inference{F,
	A1<:AbstractDeExpr,
	A2<:AbstractDeExpr}(ex::DeCall{F,(A1,A2)}) 
	
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
	A3<:AbstractDeExpr}(ex::DeCall{F,(A1,A2,A3)})
	
		t = TFun{F}()
		:( result_type(
			$t,
			$(gen_type_inference(ex.args[1])),
			$(gen_type_inference(ex.args[2])),
			$(gen_type_inference(ex.args[3])) 
		) )
end






