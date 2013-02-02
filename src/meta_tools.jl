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

gen_type_inference(ex::TNum) = :( typeof($(ex.e)) )
gen_type_inference(ex::TSym) = :( eltype($(ex.e)) )
gen_type_inference(ex::TAssign) = :( $(gen_type_inference(ex.rhs)) )
gen_type_inference(ex::TRef) = :( eltype($(ex.host)) )


function gen_type_inference(ex::TCall) 

	tf = TFun{ex.fun}()
	argty_exprs = [gen_type_inference(a) for a in ex.args]

	create_fun_call(:result_type, tf, argty_exprs...) 
end




