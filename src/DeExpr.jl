module DeExpr
	
export
	# fun_traits
	TFun,
	TCallSig,
	result_type,
	sqr, rcp, blend,

	# texpr

	DeError,

	TExpr, TEWise, TScalar, TFunCall,
	TEmpty, TNum, TScalarVar, TVar, 
	TRef, TIndex, TRange, TColon, TInterval,
	TRefScalar1, TRefScalar2, TRef1D, TRef2D, TRefRow, TRefCol,
	TMap, TReduc, TColwiseReduc, TRowwiseReduc,
	TAssign, TBlock,

	texpr, tnum, tscalarvar, tvar, to_expr, 
	tref, tcall, tassign, topassign, tblock

import Base.==, Base.!=

include("fun_traits.jl")
include("texpr.jl")
#include("meta_tools.jl")

#include("compile_base.jl")
#include("scalar_backend.jl")

end
