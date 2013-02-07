# Function traits to support delayed evaluation

type TFun{S} end
type TCallSig{S, N} end

abstract FunKind

type EWiseOp <: FunKind end
type ReducOp <: FunKind end

get_op_kind{S,N}(::TCallSig{S,N}) = nothing


##########################################################################
#
# 	op-assignment
#
#########################################################################

is_opassign(f::TFun) = false
is_opassign(::TFun{:+=}) = true
is_opassign(::TFun{:-=}) = true
is_opassign(::TFun{:.*=}) = true
is_opassign(::TFun{:./=}) = true

is_opassign(f::Symbol) = is_opassign(TFun{f}())

extract_assign_op(::TFun{:+=}) = (:+)
extract_assign_op(::TFun{:-=}) = (:-)
extract_assign_op(::TFun{:.*=}) = (:.*)
extract_assign_op(::TFun{:./=}) = (:./)


##########################################################################
#
# 	general devices to facilitate function registration
#
#########################################################################


function register_ewise_mathop(sym::Symbol, nargs::Integer)
	# the function to generate the codes to register a ewise function

	tc = TCallSig{sym, nargs}
	s1 = :( get_op_kind(::$(expr(:quote, tc))) = EWiseOp() )

	tf = TFun{sym}

	if nargs == 1
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type) = T1 )
	elseif nargs == 2
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type, T2::Type) = promote_type(T1, T2) )
	elseif nargs == 3
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type, T2::Type, T3::Type) = promote_type(promote_type(T1, T2), T3) )
	else
		error("register_ewise_mathop supports up to three arguments.")
	end

	:( $s1; $s2 )
end


function register_ewise_pred(sym::Symbol, nargs::Integer)
	# the function to generate the codes to register a ewise predicate

	tc = TCallSig{sym, nargs}
	s1 = :( get_op_kind(::$(expr(:quote, tc))) = EWiseOp() )

	tf = TFun{sym}

	if nargs == 1
		s2 = :( result_type(::$(expr(:quote, tf)), ::Type) = Bool )
	elseif nargs == 2
		s2 = :( result_type(::$(expr(:quote, tf)), ::Type, ::Type) = Bool )
	elseif nargs == 3
		s2 = :( result_type(::$(expr(:quote, tf)), ::Type, ::Type, ::Type) = Bool )
	else
		error("register_ewise_pred supports up to three arguments.")
	end

	:( $s1; $s2 )
end


function register_reductor(sym::Symbol, nargs::Integer)
	# the function to generate the codes to register a reduction function

	tc = TCallSig{sym, nargs}
	s1 = :( get_op_kind(::$(expr(:quote, tc))) = ReducOp() )

	tf = TFun{sym}

	if nargs == 1
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type) = T1 )
	elseif nargs == 2
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type, T2::Type) = promote_type(T1, T2) )
	elseif nargs == 3
		s2 = :( result_type(::$(expr(:quote, tf)), 
			T1::Type, T2::Type, T3::Type) = promote_type(promote_type(T1, T2), T3) )
	else
		error("register_reductor supports up to three arguments.")
	end

	:( $s1; $s2 )
end


##########################################################################
#
# 	registration of known operators and functions
#
#########################################################################

# arithmetics

for s in [:+, :-]
	@eval $(register_ewise_mathop(s, 1))
end

for s in [:+, :-, :.+, :.-, :.*, :./, :.^, :max, :min]
	@eval $(register_ewise_mathop(s, 2))
end

@eval $(register_ewise_mathop(:+, 3))
@eval $(register_ewise_mathop(:clamp, 3))

# comparison & logical

for s in [:.==, :.!=, :.<, :.>, :.<=, :.>= ]
	@eval $(register_ewise_pred(s, 2))
end

get_op_kind(::TCallSig{:&, 2}) = EWiseOp()
get_op_kind(::TCallSig{:|, 2}) = EWiseOp()

result_type(::TFun{:&}, ::Type{Bool}, ::Type{Bool}) = Bool
result_type(::TFun{:|}, ::Type{Bool}, ::Type{Bool}) = Bool

# math functions

sqr(x::Number) = x * x
sqr{T<:Number}(a::AbstractArray{T}) = a .* a

rcp(x::FloatingPoint) = one(x) / x
rcp{T<:FloatingPoint}(a::AbstractArray{T}) =  one(eltype(a)) ./ a

for s in [
	:sqrt, :cbrt, :abs, :sqr, :rcp,  
	:floor, :ceil, :round, :trunc,
	:exp, :log, :log10, :exp2, :log2, :expm1, :log1p, 
	:sin, :cos, :tan, :asin, :acos, :atan, 
	:sinh, :cosh, :tanh, :asinh, :acosh, :atanh,
	:erf, :erfc, :gamma, :lgamma, :digamma ]

	@eval $(register_ewise_mathop(s, 1))
end

for s in [:mod, :hypot, :atan2]
	@eval $(register_ewise_mathop(s, 2))
end

# blending

blend{T1, T2}(c::Bool, x::T1, y::T2) = (c ? x : y)

get_op_kind(::TCallSig{:blend, 3}) = EWiseOp()
result_type(::TFun{:blend}, ::Type{Bool}, T1::Type, T2::Type) = promote_type(T1, T2)


# reduction functions

for s in [ :sum, :max, :min, :mean ]
	@eval $(register_reductor(s, 1))
end

for s in [ :dot ]
	@eval $(register_reductor(s, 2))
end

# reduction function traits

reduc_result_getter(f::TFun, s::Symbol, n::Symbol) = s

reduc_initializer(f::TFun{:sum}, ty::Symbol) = :( zero($ty) )
reduc_emptyval(f::TFun{:sum}, ty::Symbol) = :( zero($ty) )
reduc_updater(f::TFun{:sum}, s::Symbol, x::Symbol) = :( $s += $x )

reduc_initializer(f::TFun{:max}, ty::Symbol) = :( typemin($ty) )
reduc_emptyval(f::TFun{:max}, ty::Symbol) = :( typemin($ty) )
reduc_updater(f::TFun{:max}, s::Symbol, x::Symbol) = :( $s = max($s, $x) )

reduc_initializer(f::TFun{:min}, ty::Symbol) = :( typemax($ty) )
reduc_emptyval(f::TFun{:min}, ty::Symbol) = :( typemax($ty) )
reduc_updater(f::TFun{:min}, s::Symbol, x::Symbol) = :( $s = min($s, $x) )

reduc_initializer(f::TFun{:mean}, ty::Symbol) = :( zero($ty) )
reduc_emptyval(f::TFun{:mean}, ty::Symbol) = :( nan($ty) )
reduc_updater(f::TFun{:mean}, s::Symbol, x::Symbol) = :( $s += $x )
reduc_result_getter(f::TFun{:mean}, s::Symbol, n::Symbol) = :( $s / $n )

reduc_initializer(f::TFun{:dot}, ty::Symbol) = :( zero($ty) )
reduc_emptyval(f::TFun{:dot}, ty::Symbol) = :( zero($ty) )
reduc_updater(f::TFun{:dot}, s::Symbol, x::Symbol, y::Symbol) = :( $s += $x * $y )



