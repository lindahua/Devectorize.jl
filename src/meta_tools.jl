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

qname(x::Symbol) = qname(:DeExpr, x)

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
# 	getting size info of LHS
#
##########################################################################

length_getter(ex::TSym) = :( length($(ex.e)) )
length_getter(ex::TRef1D) = :( length($(ex.host)) )
length_getter(ex::TRefCol) = :( size($(ex.host), 1) )
length_getter(ex::TRefRow) = :( size($(ex.host), 2) )

size2d_getter(ex::TSym) = :( size($(ex.e)) )
size2d_getter(ex::TRef2D) = :( size($(ex.host)) )

to_size2d(s::(Int,)) = (s[1], 1)
to_size2d(s::(Int, Int)) = s

to_length(s::(Int,)) = s[1]
to_length(s::(Int, Int)) = s[1] * s[2]


##########################################################################
#
# 	shape inference
#
##########################################################################

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

# the hyper-function to generate codes for size inference

size_inference(ex::TScalar) = :( () )
size_inference(ex::TSym) = :( size($(ex.e)) )

size_inference(ex::TRef1D) = :( (length($(ex.host)),) )
size_inference(ex::TRefCol) = :( (size($(ex.host),1),) )
size_inference(ex::TRefRow) = :( (1, size($(ex.host),2)) )
size_inference(ex::TRef2D) = :( size($(ex.host)) )

size_inference(ex::TAssign) = :( $(size_inference(ex.rhs)) )


function args_size_inference(args::(TEWise...,))
	if length(args) == 1
		size_inference(args[1])
	else
		arg_stmts = [size_inference(a) for a in args]
		fun_call(qname(:ewise_shape), arg_stmts...)
	end
end

size_inference(ex::TMap) = args_size_inference(ex.args)



##########################################################################
#
# 	type inference
#
##########################################################################

type_inference(ex::TNum) = :( typeof($(ex.e)) )
type_inference(ex::TSym) = :( eltype($(ex.e)) )
type_inference(ex::TRefScalar) = :( eltype($(ex.host)) )
type_inference(ex::TRef) = :( eltype($(ex.host)) )

type_inference(ex::TAssign) = :( $(type_inference(ex.rhs)) )

function type_inference(ex::TFunCall) 

	tf = TFun{ex.fun}()
	argty_exprs = [type_inference(a) for a in ex.args]

	fun_call(qname(:result_type), tf, argty_exprs...) 
end




