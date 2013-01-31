module DeExpr
	
export
	# classes to express delayed expressions
	TExpr,
	TNum,
	TInt,
	TSym,
	TRef,
	TCall,
	TAssign,

	# function traits
	TCallSig,
	TFun,
	register_ewise_mathop,
	register_ewise_pred,
	
	# helper functions
	pretty,
	de_expr,
	ewise_shape,
	result_type,
	
	# core functions
	texpr,
	de_compile,
	get,
	
	# evaluation context abstract classes
	EvalContext,
	CPUContext,
	GPUContext,
	
	# Scalar back-end
	ScalarContext,
	de_arr,
	de_col,
	de_row,
	
	# macros
	@devec,
	@inspect_devec

import Base.get
	
include("fun_traits.jl")
include("de_meta.jl")
include("de_compile_base.jl")
include("scalar_backend.jl")

end
