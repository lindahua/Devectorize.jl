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
	get,
	
	# core functions
	texpr,
	de_compile,
	
	# evaluation context abstract classes
	EvalContext,
	DirectContext,
	OffshoreContext,
	
	# Scalar back-end
	ScalarContext,
	de_arr,
	de_col,
	de_row,
	
	# macros
	@devec,
	@inspect_devec

	
include("fun_traits.jl")
include("de_meta.jl")
include("texpr_analysis.jl")
include("de_compile_base.jl")
include("scalar_backend.jl")

end
