module DeExpr
	
export
	# fun_traits
	TFun,
	TCallSig,
	result_type,

	# texpr

	DeError,

	TExpr, TEWise, TScalar, TIndex, 
	TMode, ScalarMode, EWiseMode, ReducMode, PReducMode, 
	TNum, TSym, TScalarSym, TSym,
	TRefScalar, TRefScalar1, TRefScalar2,
	TRef, TRef1D, TRef2D, TRefCol, TRefRow,
	TMap, TReduc, TPReduc, TAssign,

	tmode, 
	tnum, tsym, tscalarsym, trefscalar, 
	tref1d, tref2d, trefcol, trefrow,
	tcall, tassign,
	texpr

	
include("fun_traits.jl")
include("texpr.jl")

end
