# This file provides useful tools for meta-programming (e.g. code generation)


##########################################################################
#
# 	code construction tools
#
##########################################################################

function code_block(stmts...)
	stmts_ = Any[]
	for s in stmts
		if !(s == nothing)
			push!(stmts_, s)
		end
	end
	isempty(stmts_) ? nothing : expr(:block, stmts_)
end


function flatten_code_block_to(q, stmt_lst)
	for s in stmt_lst
		if s == nothing || s.head == (:line) 
			continue
		end
		if s.head == (:block)
			flatten_code_block_to(q, s.args)
		else
			push!(q, s)
		end
	end
end

function flatten_code_block(stmts...)
	q = Any[]
	flatten_code_block_to(q, stmts)
	code_block(q...)
end

function fun_call(funsym, args...)
	expr(:call, funsym, args...)
end

function qname(m::Symbol, x::Symbol)
	expr(:(.), m, expr(:quote, x))
end

qname(x::Symbol) = qname(:Devectorize, x)

function assignment(lhs, rhs)
	expr(:(=), lhs, rhs)
end

function if_statement(con, tblock)
	expr(:if, con, tblock)
end

function if_statement(con, tblock, fblock)
	expr(:if, con, tblock, fblock)
end

function for_statement(head, body)
	expr(:for, head, body)
end

function for_statement(i::Symbol, si::TIndex, ei::TIndex, body)
	expr(:for, :( ($i) = ($si) : ($ei) ), body)
end


##########################################################################
#
# 	shape inference
#
##########################################################################

# helper for returning length

to_length(s::Int) = s
to_length(s::(Int,)) = s[1]
to_length(s::(Int, Int)) = s[1] * s[2]

# helper for returning 2D size

to_size2d(s::Int) = (s, 1)
to_size2d(s::(Int,)) = (s[1], 1)
to_size2d(s::(Int, Int)) = s

# length inference (from LHS)

length_inference(s::Symbol) = fun_call(:length, s)

length_inference(s::Symbol, rgn::TColon) = fun_call(:length, s)

length_inference_(base_len::Expr, rgn::TColon) = base_len

function length_inference_(base_len::Expr, rgn::TInterval)
	first = rgn.first
	last = rgn.last

	blen = last == nothing ? base_len : last

	if isa(first, Int)
		if first == 1
			blen
		else
			b = first - 1
			if isa(blen, Int)
				max(blen - b, 0)
			else				
				:( max($blen - $b, 0) )
			end
		end
	else
		if isa(blen, Int)
			a = blen + 1
			:( max($a - $first, 0) )
		else		
			:( max($blen + 1 - $first, 0) )
		end
	end
end


function length_inference(s::Symbol, rgn::TInterval)
	length_inference_(fun_call(:length, s), rgn)
end


# the hyper-function to generate codes for size inference

function vec_size_inference(s::Symbol, rgn::TColon)
	:( (length($s),) )
end

function vec_size_inference(s::Symbol, rgn::TInterval)
	:( ($(length_inference(s, rgn)),) )
end

function col_size_inference(s::Symbol, rgn::TColon)
	:( (size($s, 1),) )
end

function col_size_inference(s::Symbol, rgn::TInterval)
	baselen_ = :( size($s, 1) )
	:( ($(length_inference_(baselen_, rgn)),) )
end

function row_size_inference(s::Symbol, rgn::TColon)
	:( (1, size($s, 2)) )
end

function row_size_inference(s::Symbol, rgn::TInterval)
	baselen_ = :( size($s, 2) )
	:( (1, $(length_inference_(baselen_, rgn))) )
end

function mat_size_inference(s::Symbol, rrgn::TColon, crgn::TColon)
	fun_call(qname(:to_size2d), fun_call(:size, s))
end

function mat_size_inference(s::Symbol, rrgn::TColon, crgn::TInterval)
	bn = :( size($s, 2) )
	:( (size($s, 1), $(length_inference_(bn, crgn))) )
end

function mat_size_inference(s::Symbol, rrgn::TInterval, crgn::TColon)
	bm = :( size($s, 1) )
	:( ($(length_inference_(bm, rrgn)), size($s, 2)) )
end

function mat_size_inference(s::Symbol, rrgn::TInterval, crgn::TInterval)
	bm = :( size($s, 1) )
	bn = :( size($s, 2) )
	:( ($(length_inference_(bm, rrgn)), $(length_inference_(bn, crgn))) )
end


# ewise_shape 

ewise_shape(s) = s

ewise_shape(::(), ::()) = ()
ewise_shape(::(), s) = s
ewise_shape(s, ::()) = s
ewise_shape(s1, s2) = promote_shape(s1, s2)

ewise_shape(::(), ::(), ::()) = ()
ewise_shape(s1, ::(), ::()) = s1
ewise_shape(::(), s2, ::()) = s2
ewise_shape(::(), ::(), s3) = s3
ewise_shape(s1, s2, ::()) = promote_shape(s1, s2)
ewise_shape(s1, ::(), s3) = promote_shape(s1, s3)
ewise_shape(::(), s2, s3) = promote_shape(s2, s3)
ewise_shape(s1, s2, s3) = promote_shape(promote_shape(s1, s2), s3)

ewise_shape(s1, s2, s3, s4...) = promote_shape(ewise_shape(s1, s2), ewise_shape(s3, s4...))

function ewise_size_inference(a1_siz::Expr)
	a1_siz
end

function ewise_size_inference(a1_siz::Expr, other_sizs::Expr...)
	fun_call(qname(:ewise_shape), a1_siz, other_sizs...)
end


##########################################################################
#
# 	type inference
#
##########################################################################

type_inference(ex::TNum) = :( typeof($(ex.val)) )
type_inference(ex::TVar) = :( eltype($(ex.name)) )
type_inference(ex::TScalarVar) = :( typeof($(ex.name)) )
type_inference(ex::TRef) = :( eltype($(ex.host)) )
type_inference(ex::TAssign) = :( $(type_inference(ex.rhs)) )

function result_type_inference(f::Symbol, arg_tys::Expr...) 
	tf = TFun{f}()
	fun_call(qname(:result_type), tf, arg_tys...) 
end


##########################################################################
#
# 	indexer
#
##########################################################################

indexer(rgn::TColon, i::TIndex) = i

function indexer(rgn::TInterval, i::TIndex) 
	f = rgn.first	
	if isa(f, Int)
		b = f - 1
		if b == 0
			i
		else
			:($i + $b)
		end
	else
		:($i + ($f - 1))
	end
end



