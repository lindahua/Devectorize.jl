# This file defines a hierarchy of typed expressions


##########################################################################
#
# 	Typed expressions
#
##########################################################################

# types

abstract TExpr
abstract TEWise <: TExpr
abstract TScalar <: TEWise

typealias TIndex Union(Symbol,Int)

abstract TMode
type ScalarMode <: TMode end
type EWiseMode{D} <: TMode end
type ReducMode <: TMode end
type PReducMode <: TMode end

type TEmpty <: TExpr end

type TNum{T<:Number} <: TScalar
	e::T
end

type TScalarSym <: TScalar
	e::Symbol
end

type TSym <: TEWise
	e::Symbol
end

abstract TRefScalar <: TScalar

type TRefScalar1 <: TRefScalar
	host::Symbol
	i::TIndex
end

type TRefScalar2 <: TRefScalar
	host::Symbol
	i::TIndex
	j::TIndex
end


abstract TRef <: TEWise

type TRef1D <: TRef
	host::Symbol
end

type TRef2D <: TRef
	host::Symbol
end

type TRefCol <: TRef
	host::Symbol
	icol::TIndex
end

type TRefRow <: TRef
	host::Symbol
	irow::TIndex
end

type TMap <: TEWise
	fun::Symbol
	args::(TEWise...,)
	mode::TMode
end

type TReduc <: TExpr
	fun::Symbol
	args::(TEWise...,)
	arg_mode::TMode
end

type TPReduc <: TExpr
	fun::Symbol
	args::(TEWise...,)
	dim::TIndex
end

typealias TFunCall Union(TMap,TReduc,TPReduc)


type TAssign{Lhs<:Union(TSym,TRefScalar,TRef), Rhs<:TExpr} <: TExpr
	lhs::Lhs
	rhs::Rhs
	mode::TMode
end


##########################################################################
#
# 	Determining the expression mode
#
##########################################################################

tmode_num{D}(::EWiseMode{D}) = D

tmode(ex::TScalar) = ScalarMode()
tmode(ex::TSym) = EWiseMode{0}()

tmode(ex::TRef1D) = EWiseMode{1}() 
tmode(ex::TRef2D) = EWiseMode{2}()
tmode(ex::TRefCol) = EWiseMode{1}()
tmode(ex::TRefRow) = EWiseMode{1}()

tmode(ex::TMap) = ex.mode
tmode(ex::TReduc) = isa(ex.arg_mode, ScalarMode) ? ScalarMode() : ReducMode()
tmode(ex::TPReduc) = PReducMode()

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

promote_ewise_tmode(m1::TMode, m2::TMode, 
	m3::TMode...) = promote_ewise_tmode(promote_ewise_tmode(m1, m2), promote_ewise_tmode(m3...))



##########################################################################
#
# 	construction functions
#
##########################################################################

type DeError <: Exception
	msg::ASCIIString
end

tnum(x::Number) = TNum{typeof(x)}(x)
tsym(s::Symbol) = TSym(s)
tscalarsym(s::Symbol) = TScalarSym(s)

trefscalar(x::Symbol, i::TIndex) = TRefScalar1(x, i)
trefscalar(x::Symbol, i::TIndex, j::TIndex) = TRefScalar2(x, i, j)

tref1d(x::Symbol) = TRef1D(x)
tref2d(x::Symbol) = TRef2D(x)
trefcol(x::Symbol, i::TIndex) = TRefCol(x, i)
trefrow(x::Symbol, i::TIndex) = TRefRow(x, i)


is_ewise_call(f::Symbol, N::Int) = isa(get_op_kind(TCallSig{f, N}()), EWiseOp)
is_reduc_call(f::Symbol, N::Int) = isa(get_op_kind(TCallSig{f, N}()), ReducOp)

function check_all_ewise_args(args::(TExpr...,))
	if !all([isa(a, TEWise) for a in args])
		throw(DeError("Unexpected non-ewise arguments"))
	end
end

# due to intricate semantics of partial reduction in Julia syntax
# we here hand-coded the specific form to be recognized as partial reduction

function recognize_partial_reduction(f::Symbol, a::(TExpr...,))
	if f == (:sum) || f == (:mean)
		if length(a) == 2 && (isa(a[2], TSym) || isa(a[2], TNum))
			TPReduc(f, (a[1],), a[2].e)
		end
	elseif f == (:max) || f == (:min)
		if length(a) == 3 && isa(a[2], TEmpty) && (isa(a[3], TSym) || isa(a[3], TNum))
			TPReduc(f, (a[1],), a[3].e)
		end
	end
end


function tcall(f::Symbol, args::(TExpr...,))
	n = length(args)
	if is_ewise_call(f, n)
		check_all_ewise_args(args)
		mode = promote_ewise_tmode([tmode(a) for a in args]...)
		TMap(f, args, mode)

	elseif is_reduc_call(f, n)
		check_all_ewise_args(args)
		arg_mode = promote_ewise_tmode([tmode(a) for a in args]...)
		TReduc(f, args, arg_mode)

	else
		ex = recognize_partial_reduction(f, args)
		if ex == nothing
			throw(DeError("Unrecognized function $f with $n arguments (in DeExpr)"))
		end
		return ex
	end
end


function tassign(lhs::TExpr, rhs::TExpr)

	# TScalarSym can only be created internally, which would never
	# be placed on the left hand side
	@assert !isa(lhs, TScalarSym)

	if isa(lhs, TSym)
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
		mode = promote_ewise_tmode(tmode(lhs), tmode(rhs))

	else
		throw(DeError("Incompatible modes between lhs and rhs."))
	end

	TAssign{typeof(lhs),typeof(rhs)}(lhs, rhs, mode)
end


##########################################################################
#
# 	AST construction:  Expr ==> TExpr
#
##########################################################################

texpr(x::Number) = tnum(x)
texpr(x::Symbol) = tsym(x)

function check_simple_ref(c::Bool)
	if !c
		throw(DeError("non-simple ref-expression is not supported"))
	end
end

function texpr_for_ref(ex::Expr)
	@assert ex.head == :(ref)

	na = length(ex.args)
	check_simple_ref(na == 2 || na == 3)

	hsym = ex.args[1]
	check_simple_ref(isa(hsym, Symbol))

	if na == 2
		a1 = ex.args[2]
		check_simple_ref(isa(a1, TIndex))

		a1 == :(:) ? tref1d(hsym) : trefscalar(hsym, a1)

	else 
		a1 = ex.args[2]
		a2 = ex.args[3]
		check_simple_ref(isa(a1, TIndex))
		check_simple_ref(isa(a2, TIndex))

		if a1 == :(:)
			a2 == :(:) ? tref2d(hsym) : trefcol(hsym, a2)
		else
			a2 == :(:) ? trefrow(hsym, a1) : trefscalar(hsym, a1, a2)
		end
	end
end


function texpr(ex::Expr) 

	if ex.head == :(call)

		fsym = ex.args[1]
		if !isa(fsym, Symbol)
			throw(DeError("call-expressions with non-symbol function name: $fsym"))
		end
		tcall(fsym, map(texpr, tuple(ex.args[2:]...)))
		
	elseif ex.head == :(ref)

		texpr_for_ref(ex)

	elseif ex.head == :(tuple) && isempty(ex.args)

		TEmpty()

	elseif ex.head == :(=)

		@assert length(ex.args) == 2
		lhs = texpr(ex.args[1])
		rhs = texpr(ex.args[2])
		tassign(lhs, rhs)

	else
		throw(DeError("Unrecognized expression: $ex"))
	end
end


