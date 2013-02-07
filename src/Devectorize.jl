module Devectorize
	
export
	# fun_traits
	TFun,
	TCallSig,
	result_type,
	sqr, rcp, blend,

	# texpr

	DeError,

	TExpr, TEWise, TScalar, TGeneralVar, TFunCall,
	TEmpty, TNum, TScalarVar, TVar, TQVar, TGeneralScalar, 
	TRef, TIndex, TRange, TColon, TInterval,
	TGeneralRef1, TGeneralRef2, TRef1D, TRef2D, TRefRow, TRefCol,
	TMap, TReduc, TColwiseReduc, TRowwiseReduc,
	TLValue, TRValue, TAssign, TBlock,

	texpr, tnum, tscalarvar, tvar, tqvar,
	tref, tcall, tassign, topassign, tblock,
	is_trivial_assignment, ju_expr,

	tmode, ScalarMode, EWiseMode, ReducMode, 
	ColwiseReducMode, RowwiseReducMode,

	# scalar_backend

	ScalarContext, dump_devec,
	compile, @devec, @inspect_devec,
	
	# extensions
	
	@devec_transform

import Base.==, Base.!=

include("fun_traits.jl")
include("texpr.jl")
include("meta_tools.jl")

include("compile_base.jl")
include("scalar_backend.jl")

include("extensions.jl")

end
