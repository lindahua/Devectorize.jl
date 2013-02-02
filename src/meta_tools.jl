# This file provides useful tools for meta-programming (e.g. code generation)


##########################################################################
#
# 	code construction tools
#
##########################################################################

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
ewise_shape(s1, ::(), ::()) = s2
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

function size_inference(ex::TMap) 
	arg_stmts = [size_inference(a) for a in ex.args]
	create_fun_call(:ewise_shape, arg_stmts...)
end


##########################################################################
#
# 	type inference
#
##########################################################################

type_inference(ex::TNum) = :( typeof($(ex.e)) )
type_inference(ex::TSym) = :( eltype($(ex.e)) )
type_inference(ex::TRefScalar) = :( eltype($(ex.host)) )
type_inference(ex::TRef) = :( eltype($(ex.host)) )

type_inference(ex::TAssign) = :( $(gen_type_inference(ex.rhs)) )

function type_inference(ex::TFunCall) 

	tf = TFun{ex.fun}()
	argty_exprs = [type_inference(a) for a in ex.args]

	create_fun_call(:result_type, tf, argty_exprs...) 
end




