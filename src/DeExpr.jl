module DeExpr
	
export
	# classes to express delayed expressions
	AbstractDeExpr,
	DeNumber,
	DeTerminal,
	DeFunExpr,
	TFun,
	
	# helper functions
	fsym,
	pretty,
	de_expr,
	ewise_shape,
	result_type,
	
	# core functions
	de_wrap,
	de_generate,
	get,
	
	# evaluation context abstract classes
	EvalContext,
	CPUContext,
	GPUContext,
	
	# Scalar back-end
	ScalarContext,
	DeConst,
	DeVecReader,
	devec_reader,
	
	# macros
	@devec
	
include("de_eval_base.jl")
include("scalar_backend.jl")

end
