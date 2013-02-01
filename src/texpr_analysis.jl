# functions to analyze (extract properties) of T-expressions

#####################################################################
#
#  expression kinds
#
#####################################################################

abstract TExprKind

type EWise{N} <: TExprKind end
type Reduc{N} <: TExprKind end
type PartialReduc <: TExprKind end

te_ndims{N}(::EWise{N}) = N
te_ndims{N}(::Reduc{N}) = N

#####################################################################
#
#  resolution of expression kind
#
#####################################################################

texpr_kind(ex::TNum) = EWise{0}()
texpr_kind(ex::TSym) = EWise{0}()

texpr_kind(ex::TRef{(TColon,)}) = EWise{1}()
texpr_kind(ex::TRef{(TColon,TInt)}) = EWise{1}()
texpr_kind(ex::TRef{(TColon,TSym)}) = EWise{1}()
texpr_kind(ex::TRef{(TInt,TColon)}) = EWise{1}()
texpr_kind(ex::TRef{(TSym,TColon)}) = EWise{1}()
texpr_kind(ex::TRef{(TColon,TColon)}) = EWise{2}()

promote_ewise_kind(a1::EWise) = a1

function promote_ewise_kind{N1, N2}(a1::EWise{N1}, a2::EWise{N2})
	N1 == N2 ? a1 :
	N1 == 0 ? a2 :
	N2 == 0 ? a1 :
	throw(DeError("Inconsistent element-wise kind"))
end

promote_ewise_kind(a1::EWise, a2::EWise, 
	a3::EWise...) =  promote_ewise_kind(promote_ewise_kind(a1, a2), promote_ewise_kind(a3...))


function texpr_kind(ex::TCall)
	# resolve the ewise kind 

	is_reduc = is_reduc_call(ex)
	is_ewise = is_ewise_call(ex)

	if is_ewise == is_reduc
		throw(DeError("Unsupported call expression"))
	end

	arg_kinds = [texpr_kind(a) for a in ex.args]
	for k in arg_kinds
		if !isa(k, EWise)
			throw(DeError("All args should be of ewise kind in a call expression"))
		end
	end

	pm_kind = promote_ewise_kind(arg_kinds...)
	N = te_ndims(pm_kind)

	is_ewise ? EWise{N}() : Reduc{N}()
end

function texpr_kind(ex::TAssign)
	if isa(ex.lhs, TSym)
		texpr_kind(ex.rhs)

	elseif isa(ex.lhs, TRef)
		lkind = texpr_kind(ex.lhs)
		rkind = texpr_kind(ex.rhs)

		if !isa(rkind, EWise)
			throw(DeError("Inconsistent expression kind between lhs and rhs"))
		end

		promote_ewise_kind(lkind, rkind)

	else
		throw(DeError("lhs can only be either a symbol or a ref expression"))
	end
end


#####################################################################
#
#  integrated analysis
#
#####################################################################

function analyze_expr(top_expr::TAssign)

	ekind_ = texpr_kind(top_expr)

	expr_kind = 
		isa(ekind_, EWise{0}) ? EWise{1}() :  
		isa(ekind_, Reduc{0}) ? Reduc{1}() :
		ekind_

	require_lhs_init = isa(top_expr.lhs, TSym)

	(expr_kind, require_lhs_init)
end




