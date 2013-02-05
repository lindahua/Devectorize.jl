# This file defines a hierarchy of typed expressions


##########################################################################
#
# 	Typed expressions
#
##########################################################################

# modes: the role/functionality of an expression

abstract TMode
type ScalarMode <: TMode end
type EWiseMode{D} <: TMode end
type ReducMode <: TMode end
type ColwiseReducMode <: TMode end
type RowwiseReducMode <: TMode end

# types

abstract TExpr
abstract TEWise <: TExpr
abstract TScalar <: TEWise

type TEmpty <: TExpr end

# simple expressions

type TNum{T<:Number} <: TScalar
	val::T
end

== (u::TNum, v::TNum) = (u.val == v.val)
!= (u::TNum, v::TNum) = !(u == v)

type TScalarVar <: TScalar
	name::Symbol
end

== (u::TScalarVar, v::TScalarVar) = (u.name == v.name)
!= (u::TScalarVar, v::TScalarVar) = !(u == v)

type TVar <: TEWise
	form::Union(Symbol,Expr)
end

== (u::TVar, v::TVar) = (u.form == v.form)
!= (u::TVar, v::TVar) = !(u == v)


# references

abstract TRef <: TEWise

typealias TIndex Union(Symbol,Int)

abstract TRange

type TColon <: TRange end
type TInterval <: TRange
	first::TIndex
	last::Union(TIndex,Nothing)
end

== (u::TInterval, v::TInterval) = (u.first == v.first) && (u.last == v.last)
!= (u::TInterval, v::TInterval) = !(u == v)

abstract TRefScalar <: TScalar

type TRefScalar1 <: TRefScalar
	host::TVar
	i::TIndex
end

type TRefScalar2 <: TRefScalar
	host::TVar
	i::TIndex
	j::TIndex
end

type TRef1D <: TRef
	host::TVar
	rgn::TRange
end

type TRefCol <: TRef
	host::TVar
	rrgn::TRange
	icol::TIndex
end

type TRefRow <: TRef
	host::TVar
	irow::TIndex
	crgn::TRange
end

type TRef2D <: TRef
	host::TVar
	rrgn::TRange
	crgn::TRange
end

# function calls

type TMap <: TEWise
	fun::Symbol
	args::(TEWise...,)
	mode::TMode
	deps::Union(Array{TExpr}, Nothing)
end

type TReduc <: TExpr
	fun::Symbol
	args::(TEWise...,)
	arg_mode::TMode
	deps::Union(Array{TExpr}, Nothing)
end

type TColwiseReduc <: TExpr
	fun::Symbol
	args::(TEWise...,)
	deps::Union(Array{TExpr}, Nothing)
end

type TRowwiseReduc <: TExpr
	fun::Symbol
	args::(TEWise...,)
	deps::Union(Array{TExpr}, Nothing)
end

typealias TFunCall Union(TMap, TReduc, TColwiseReduc, TRowwiseReduc)


# others

type TAssign{Lhs<:Union(TVar,TRefScalar,TRef), Rhs<:TExpr} <: TExpr
	lhs::Lhs
	rhs::Rhs
	mode::TMode
end

type TBlock <: TExpr
	stmts::Array{TExpr}
	TBlock() = new(TExpr[])
end


##########################################################################
#
# 	Determining the expression mode
#
#	i.e. what kind of things that the whole expression wants to do
#	(ewise, reduc, etc)
#
##########################################################################

tmode_num{D}(::EWiseMode{D}) = D

tmode(ex::TScalar) = ScalarMode()
tmode(ex::TVar) = EWiseMode{0}()

tmode(ex::TRef1D) = EWiseMode{1}() 
tmode(ex::TRef2D) = EWiseMode{2}()
tmode(ex::TRefCol) = EWiseMode{1}()
tmode(ex::TRefRow) = EWiseMode{1}()

tmode(ex::TMap) = ex.mode
tmode(ex::TReduc) = isa(ex.arg_mode, ScalarMode) ? ScalarMode() : ReducMode()
tmode(ex::TColwiseReduc) = ColwiseReducMode()
tmode(ex::TRowwiseReduc) = RowwiseReducMode()

tmode(ex::TAssign) = ex.mode

promote_ewise_tmode(m::TMode) = m
promote_ewise_tmode(m1::ScalarMode, m2::ScalarMode) = ScalarMode()
promote_ewise_tmode(m1::ScalarMode, m2::EWiseMode) = EWiseMode{tmode_num(m2)}()
promote_ewise_tmode(m1::EWiseMode, m2::ScalarMode) = EWiseMode{tmode_num(m1)}()

function promote_ewise_tmode(m1::EWiseMode, m2::EWiseMode)
	d1 = tmode_num(m1)
	d2 = tmode_num(m2)

	d1 == d2 ? EWiseMode{d1}() :
	d1 == 0 ? EWiseMode{d2}() :
	d2 == 0 ? EWiseMode{d1}() :
	throw(DeError("Incompatible ewise mode."))
end

promote_ewise_tmode(m1::TMode, m2::TMode, m3::TMode) = promote_ewise_tmode(promote_ewise_tmode(m1, m2), m3)



##########################################################################
#
# 	functions to construct simple components of a typed expression
#
##########################################################################

type DeError <: Exception
	msg::ASCIIString
end

# number literals

tnum(x::Number) = TNum{typeof(x)}(x)

# variable  (can be "a" or "a.b.c")

tvar(s::Symbol) = TVar(s)

is_valid_var(v::Symbol) = true
is_valid_var(v::Expr) = v.head == :(.) && 
	is_valid_var(v.args[1]) && 
	isa(v.args[2], Expr) && 
	v.args[2].head == :quote &&
	isa(v.args[2].args[1], Symbol)

function tvar(ex::Expr) 
	if !is_valid_var(ex)
		throw(DeError("Unrecognized variable form: $ex"))
	end
	TVar(ex)
end

tscalarvar(s::Symbol) = TScalarVar(s)

# reference expressions

function check_simple_ref(c::Bool)
	if !c
		throw(DeError("non-simple ref-expression is not supported"))
	end
end

tref_arg(i::Int) = i
tref_arg(i::Symbol) = i == :(:) ? TColon() : i

function tref_arg(ex::Expr)
	check_simple_ref(ex.head == :(:) && length(ex.args) == 2)
	a1 = ex.args[1]
	a2 = ex.args[2]
	
	if isa(a1, Int) || (isa(a1, Symbol) && a1 != :(:))
		first = a1
	else
		check_simple_ref(false)
	end

	if isa(a2, Int)
		last = a2
	elseif isa(a2, Symbol)
		last = a2 == :(:) ? nothing : a2
	else
		check_simple_ref(false)
	end

	TRange(first, last)
end

tref(x::TVar, i::TIndex) = TRefScalar1(x, i)
tref(x::TVar, r::TRange) = TRef1D(x, r)

tref(x::TVar, i::TIndex, j::TIndex) = TRefScalar2(x, i, j)
tref(x::TVar, i::TIndex, c::TRange) = TRefRow(x, i, c)
tref(x::TVar, r::TRange, j::TIndex) = TRefCol(x, r, j)
tref(x::TVar, r::TRange, c::TRange) = TRef2D(x, r, c)

function tref(ex::Expr)
	@assert ex.head == :(ref)

	na = length(ex.args)
	check_simple_ref(na == 2 || na == 3)

	h = tvar(ex.args[1])

	if na == 2
		a1 = ex.args[2]
		tref(h, tref_arg(a1))

	else 
		a1 = ex.args[2]
		a2 = ex.args[3]
		tref(h, tref_arg(a1), tref_arg(a2))
	end
end


##########################################################################
#
# 	function call recognition
#
##########################################################################

is_ewise_call(f::Symbol, N::Int) = isa(get_op_kind(TCallSig{f, N}()), EWiseOp)
is_reduc_call(f::Symbol, N::Int) = isa(get_op_kind(TCallSig{f, N}()), ReducOp)

function check_funcall_args(args::TExpr...)
	na = length(args)
	deps = nothing

	pargs = Array(TExpr, na)

	for i = 1 : na
		a = args[i]
		if isa(a, TEWise)
			pargs[i] = a
		elseif isa(a, TReduc) || isa(a, TColwiseReduc) || isa(a, TRowwiseReduc)
			dep_sym = gensym("dep")
			pargs[i] = isa(a, TReduc) ? TScalarSym(dep_sym) : TVar(dep_sym)
			if deps == nothing
				deps = TExpr[]
			end
			push!(deps, tassign(TVar(dep_sym), a))
		else
			throw(DeError("Arguments in unsupported form."))
		end
	end
	(tuple(pargs...), deps)
end

# due to intricate semantics of partial reduction in Julia syntax
# we here hand-coded the specific form to be recognized as partial reduction

function recognize_partial_reduction(f::Symbol, a::TExpr...)
	if f == (:sum) || f == (:mean)
		if length(a) == 2 && isa(a[2], TNum{Int})
			fargs, deps = check_funcall_args(a[1])
			dim = a[2].e
			if dim == 1
				TColwiseReduc(f, fargs, deps)
			elseif dim == 2
				TRowwiseReduc(f, fargs, deps)
			else
				throw(DeError("DeExpr supports either colwise or rowwise reduction."))
			end
		end
	elseif f == (:max) || f == (:min)
		if length(a) == 3 && isa(a[2], TEmpty) && isa(a[3], TNum{Int})
			fargs, deps = check_funcall_args(a[1])
			dim = a[3].e
			if dim == 1
				TColwiseReduc(f, fargs, deps)
			elseif dim == 2
				TRowwiseReduc(f, fargs, deps)
			else
				throw(DeError("DeExpr supports either colwise or rowwise reduction."))
			end
		end
	end
end


function tcall(f::Symbol, args)
	n = length(args)
	if is_ewise_call(f, n)
		fargs, deps = check_funcall_args(args...)
		mode = promote_ewise_tmode([tmode(a) for a in fargs]...)
		TMap(f, fargs, mode, deps)

	elseif is_reduc_call(f, n)
		fargs, deps = check_funcall_args(args...)
		arg_mode = promote_ewise_tmode([tmode(a) for a in fargs]...)
		TReduc(f, fargs, arg_mode, deps)

	elseif f == :(*)
		throw(DeError("DeExpr does not support *, please use .* to express element-wise multiplication."))
		
	elseif f == :(/)
		throw(DeError("DeExpr does not support /, please use ./ to express element-wise division."))
		
	elseif f == :(^)
		throw(DeError("DeExpr does not support ^, please use .^ to express element-wise power."))

	else
		ex = recognize_partial_reduction(f, args...)
		if ex == nothing
			throw(DeError("Unrecognized function $f with $n arguments (in DeExpr)"))
		end
		return ex
	end
end

function tcall(ex::Expr)
	@assert ex.head == :(call)
	fsym = ex.args[1]
	if !isa(fsym, Symbol)
		throw(DeError("call-expressions with non-symbol function name: $fsym"))
	end
	tcall(fsym, map(texpr, ex.args[2:]))
end

function tcomparison(ex::Expr)
	@assert ex.head == :(comparison)
	opsym = ex.args[2]
	tcall(opsym, map(texpr, [ex.args[1], ex.args[3]]))
end




##########################################################################
#
# 	assignment construction
#
##########################################################################

function decide_assign_tmode(lhs::TExpr, rhs::TExpr)

	# TScalarSym can only be created internally, which would never
	# be placed on the left hand side
	@assert !isa(lhs, TScalarSym)

	if isa(lhs, TVar)
		mode = tmode(rhs)

	elseif isa(lhs, TRefScalar)
		rmode = tmode(rhs)
		if isa(rmode, ScalarMode) || isa(rmode, EWiseMode{0})
			mode = ScalarMode()
		elseif isa(rmode, ReducMode)
			mode = ReducMode()
		else
			throw(DeError("rhs cannot contain non-scalar ref when lhs is a scalar-ref."))
		end

	elseif isa(lhs, TRef)
		@assert isa(lhs, TEWise)
		rmode = tmode(rhs)
		if isa(rmode, EWiseMode) || isa(rmode, ScalarMode)
			mode = promote_ewise_tmode(tmode(lhs), rmode)
		elseif isa(rmode, ColwiseReducMode) || isa(rmode, RowwiseReducMode)
			mode = rmode
		else
			throw(DeError("Incompatible lhs and rhs."))
		end

	else
		throw(DeError("Incompatible modes between lhs and rhs."))
	end
end


function tassign(lhs::TExpr, rhs::TExpr)
	if isa(rhs, TAssign)
		throw(DeError("chained assignment is not supported by DeExpr"))
	end
	mode = decide_assign_tmode(lhs, rhs)
	TAssign{typeof(lhs),typeof(rhs)}(lhs, rhs, mode)
end


function topassign(aop::Symbol, lhs::TExpr, rhs::TExpr)
	op = extract_assign_op(aop)
	new_rhs = tcall(op, [lhs, rhs])
	tassign(lhs, new_rhs)
end


function tassign(ex::Expr)
	@assert ex.head == :(=)
	@assert length(ex.args) == 2

	lhs = ex.args[1]
	rhs = ex.args[2]
	tassign(texpr(lhs), texpr(rhs))
end

function topassign(ex::Expr)
	topassign(ex.head, texpr(lhs), texpr(rhs))
end


##########################################################################
#
# 	block construction
#
##########################################################################


function tblock(blk_ex::Expr)

	@assert blk_ex.head == :(block)

	blk = TBlock()
	for e in ex.args
		if isa(e, LineNumberNode) || e.head == (:line)
			continue
		end
		if !(e.head == :(=) || ex.head == :(block))
			throw(DeError("Each statement in a block must be an assignment or a nested block"))
		end
		push!(blk.stmts, texpr(e))
	end
	blk
end



##########################################################################
#
# 	AST construction:  Expr ==> TExpr
#
##########################################################################

texpr(x::Number) = tnum(x)
texpr(x::Symbol) = tvar(x)

is_empty_tuple(ex::Expr) = ex.head == :(tuple) && isempty(ex.args)

texpr(ex::Expr) = 
	ex.head == :(call) ? tcall(ex) :
	ex.head == :(comparison) ? tcomparison(ex) :
	ex.head == :(ref) ? tref(ex) :
	ex.head == :(=) ? tassign(ex) :
	ex.head == :(block) ? tblock(ex) :
	is_empty_tuple(ex) ? TEmpty() :
	is_opassign(ex.head) ? topassign(ex) :
	throw(DeError("Unrecognized expression: $ex"))



