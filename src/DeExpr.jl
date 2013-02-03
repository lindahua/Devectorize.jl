module DeExpr
	
export
	# fun_traits
	TFun,
	TCallSig,
	result_type,
	sqr, rcp,

	# texpr

	DeError,

	TExpr, TEWise, TScalar, TIndex, 
	TMode, ScalarMode, EWiseMode, ReducMode, PReducMode, 
	TNum, TSym, TScalarSym, TSym,
	TRefScalar, TRefScalar1, TRefScalar2,
	TRef, TRef1D, TRef2D, TRefCol, TRefRow,
	TMap, TReduc, TPReduc, TFunCall, TAssign,

	tmode, 
	tnum, tsym, tscalarsym, trefscalar, 
	tref1d, tref2d, trefcol, trefrow,
	tcall, tassign,
	texpr,

	# compile_base

	EvalContext,
	DirectContext,
	OffshoreContext,

	compile,
	compile_fast_reduc,

	# scalar_backend

	ScalarContext,
	de_arr,
	de_col,
	de_row,

	# macros
	@devec,
	@inspect_devec,
	@fast_reduc,
	@inspect_fast_reduc

	
include("fun_traits.jl")
include("texpr.jl")
include("meta_tools.jl")

include("compile_base.jl")
include("scalar_backend.jl")

end
