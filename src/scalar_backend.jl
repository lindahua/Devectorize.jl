
type ScalarContext <: DirectContext  # de-vectorized scalar for-loop
end

##########################################################################
#
#   array access helpers
#
##########################################################################

# const

get_value(r::Number, ::Int) = r
get_value(r::Number, ::Int, ::Int) = r

# vector reader

get_value{T<:Number}(r::Array{T}, i::Int) = r[i]
get_value{T<:Number}(r::Array{T}, i::Int, j::Int) = r[i, j]
get_value{T<:Number}(r::AbstractArray{T}, i::Int) = r[i]
get_value{T<:Number}(r::AbstractArray{T}, i::Int, j::Int) = r[i, j]


##########################################################################
#
#   Assignment composition
#
##########################################################################


function compose_kernel(ctx::ScalarContext, ex::TAssign, lhs_info, rhs_info, i::Symbol)

    lhs_kernel = compose_lhs_kernel(ctx, ex.lhs, lhs_info, i)
    rhs_kernel = compose_rhs_kernel(ctx, ex.rhs, rhs_info, i)
    assignment(lhs_kernel, rhs_kernel)
end

function compose_kernel(ctx::ScalarContext, ex::TAssign, lhs_info, rhs_info, i::Symbol, j::Symbol)

    lhs_kernel = compose_lhs_kernel(ctx, ex.lhs, lhs_info, i, j)
    rhs_kernel = compose_rhs_kernel(ctx, ex.rhs, rhs_info, i, j)
    assignment(lhs_kernel, rhs_kernel)
end


##########################################################################
#
#   LHS composition
#
##########################################################################

# general setup routine for symbol replacement

function with_sym_replacement(ex::TGeneralVar)
    if isa(ex, TVar)
        (nothing, ex.name)
    else
        t = gensym("t")
        pre = assignment(t, ju_expr(ex))
        (pre, t)
    end
end


# TVar

function setup_lhs(ctx::ScalarContext, ex::TVar)
    (nothing, nothing)
end

function length_getter(ctx::ScalarContext, lhs::TVar, info::Nothing)
    fun_call(:length, lhs.name)
end

function size2d_getter(ctx::ScalarContext, lhs::TVar, info::Nothing)
    fun_call(qname(:to_size2d), fun_call(:size, lhs.name))
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol)
    :( $(ex.name)[$i] )
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol, j::Symbol)
    :( $(ex.name)[$i, $j] )
end

# TRef1D

function setup_lhs(ctx::ScalarContext, ex::TRef1D)
    with_sym_replacement(ex.host)
end

function length_getter(ctx::ScalarContext, lhs::TRef1D, h::Symbol)
    length_inference(h, lhs.rgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRef1D, h::Symbol, i::Symbol)
    :( $(h)[$(indexer(ex.rgn, i))] )
end

# TRefCol

function setup_lhs(ctx::ScalarContext, ex::TRefCol)
    (hpre, h) = with_sym_replacement(ex.host)
    if isa(ex.icol, Int)
        icol = ex.icol
        pre = hpre
    else
        icol = gensym("icol")
        pre = code_block(hpre,
            assignment(icol, fun_call(:convert, :Int, ex.icol)) )
    end
    (pre, (h, icol))
end

function length_getter(ctx::ScalarContext, lhs::TRefCol, info)
    baselen = fun_call(:size, info[1], 1)
    length_inference_(baselen, lhs.rrgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRefCol, info, i::Symbol)
    h, icol = info
    :( $(h)[$(indexer(ex.rrgn, i)), $icol] )
end

# TRefRow

function setup_lhs(ctx::ScalarContext, ex::TRefRow)
    (hpre, h) = with_sym_replacement(ex.host)
    if isa(ex.irow, Int)
        irow = ex.irow
        pre = hpre
    else
        irow = gensym("irow")
        pre = code_block(hpre,
            assignment(irow, fun_call(:convert, :Int, ex.irow)) )
    end
    (pre, (h, irow))
end

function length_getter(ctx::ScalarContext, lhs::TRefRow, info)
    baselen = fun_call(:size, info[1], 2)
    length_inference_(baselen, lhs.crgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRefRow, info, i::Symbol)
    h, irow = info
    :( $(h)[$irow, $(indexer(ex.crgn, i))] )
end

# TRef2D

function setup_lhs(ctx::ScalarContext, ex::TRef2D)
    with_sym_replacement(ex.host)
end

function size2d_getter(ctx::ScalarContext, lhs::TRef2D, h::Symbol)
    mat_size_inference(h, lhs.rrgn, lhs.crgn)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TRef2D, h::Symbol, i::Symbol, j::Symbol)
    :( $(h)[$(indexer(ex.rrgn, i)), $(indexer(ex.crgn, j))] )
end

# TGeneralRef1

function setup_lhs(ctx::ScalarContext, ex::TGeneralRef1)
    hpre, h = with_sym_replacement(ex.host)
    # insert code to convert to integer index
    I = gensym("I")
    pre = code_block(hpre,
        assignment(I, fun_call(:(Base.to_index), ex.i)) )
    (pre, (h, I))
end

function length_getter(ctx::ScalarContext, lhs::TGeneralRef1, info)
    h, I = info
    length_inference(I)
end

function compose_lhs_kernel(ctx::ScalarContext, ex::TGeneralRef1, info, i::Symbol)
    h, I = info
    :( $(h)[$(I)[$(i)]] )
end


##########################################################################
#
#   RHS composition
#
##########################################################################

# TNum

function setup_rhs(ctx::ScalarContext, ex::TNum)
    init = nothing
    siz = :()
    ty = fun_call(:typeof, ex.val)
    final = nothing
    info = nothing
    (init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TNum, info::Nothing, i::Symbol)
    ex.val
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TNum, info::Nothing, i::Symbol, j::Symbol)
    ex.val
end

# TVar

function setup_rhs(ctx::ScalarContext, ex::TVar)
    init = nothing
    siz = fun_call(:size, ex.name)
    ty = fun_call(:eltype, ex.name)
    final = nothing
    info = nothing
    (init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol)
    fun_call(qname(:get_value), ex.name, i)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TVar, info::Nothing, i::Symbol, j::Symbol)
    fun_call(qname(:get_value), ex.name, i, j)
end

# TScalarVar

function setup_rhs(ctx::ScalarContext, ex::TScalarVar)
    init = nothing
    siz = :()
    ty = fun_call(:typeof, ex.name)
    final = nothing
    info = nothing
    (init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TScalarVar, info::Nothing, i::Symbol)
    ex.name
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TScalarVar, info::Nothing, i::Symbol, j::Symbol)
    ex.name
end

# other TGeneralVar (e.g. TScalarRef1, TGeneralRef2)

function setup_rhs(ctx::ScalarContext, ex::TGeneralVar)
    t = gensym("t")
    init = assignment(t, ju_expr(ex))
    siz = fun_call(:size, t)
    ty = fun_call(:eltype, t)
    final = nothing
    (init, siz, ty, final, t)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TGeneralVar, t::Symbol, i::Symbol)
    fun_call(qname(:get_value), t, i)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TGeneralVar, t::Symbol, i::Symbol, j::Symbol)
    fun_call(qname(:get_value), t, i, j)
end

# TScalarRef1

compose_rhs_kernel(ctx::ScalarContext, ex::TScalarRef1, t::Symbol, i::Symbol) = t

# TGeneralRef1

function setup_rhs(ctx::ScalarContext, ex::TGeneralRef1)
    (init, info) = setup_lhs(ctx, ex)
    (h, I) = info
    siz = vec_size_inference(h, I)
    ty = fun_call(:eltype, h)
    final = nothing
    (init, siz, ty, final, info)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TGeneralRef1, info, i::Symbol)
    (h, I) = info
    :( $(h)[$(fun_call(qname(:get_value), I, i))] )
end

# TRef1D, TRefCol, TRefRow

function setup_rhs(ctx::ScalarContext, ex::TRef1D)
    (init, h) = setup_lhs(ctx, ex)
    siz = vec_size_inference(h, ex.rgn)
    ty = fun_call(:eltype, h)
    final = nothing
    (init, siz, ty, final, h)
end

function setup_rhs(ctx::ScalarContext, ex::TRefCol)
    (init, info) = setup_lhs(ctx, ex)
    h = info[1]
    siz = col_size_inference(h, ex.rrgn)
    ty = fun_call(:eltype, h)
    final = nothing
    (init, siz, ty, final, info)
end

function setup_rhs(ctx::ScalarContext, ex::TRefRow)
    (init, info) = setup_lhs(ctx, ex)
    h = info[1]
    siz = row_size_inference(h, ex.crgn)
    ty = fun_call(:eltype, h)
    final = nothing
    (init, siz, ty, final, info)
end

compose_rhs_kernel(ctx::ScalarContext, ex::Union(TRef1D, TRefCol, TRefRow), info,
    i::Symbol) = compose_lhs_kernel(ctx, ex, info, i)


# TRef2D

function setup_rhs(ctx::ScalarContext, ex::TRef2D)
    (init, h) = setup_lhs(ctx, ex)
    siz = mat_size_inference(h, ex.rrgn, ex.crgn)
    ty = fun_call(:eltype, h)
    final = nothing
    (init, siz, ty, final, h)
end

compose_rhs_kernel(ctx::ScalarContext, ex::TRef2D, h::Symbol,
    i::Symbol, j::Symbol) = compose_lhs_kernel(ctx, ex, h, i, j)


# TMap

function setup_args(ctx::ScalarContext, f::Symbol, args)
    # setup arguments

    arg_setups = [setup_rhs(ctx, a) for a in args]

    arg_inits = [s[1] for s in arg_setups]
    arg_sizes = [s[2] for s in arg_setups]
    arg_types = [s[3] for s in arg_setups]
    arg_finals = [s[4] for s in arg_setups]
    arg_infos = [s[5] for s in arg_setups]

    # integrate

    init = code_block(arg_inits...)
    siz = ewise_size_inference(arg_sizes...)
    ty = result_type_inference(f, arg_types...)
    final = code_block(arg_finals...)

    (init, siz, ty, final, arg_infos)
end

function setup_rhs(ctx::ScalarContext, ex::TMap)
    setup_args(ctx, ex.fun, ex.args)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TMap, arg_infos, i::Symbol)
    na = length(ex.args)
    arg_kernels = [compose_rhs_kernel(ctx, ex.args[ia], arg_infos[ia], i) for ia in 1 : na]
    fun_call(ex.fun, arg_kernels...)
end

function compose_rhs_kernel(ctx::ScalarContext, ex::TMap, arg_infos, i::Symbol, j::Symbol)
    na = length(ex.args)
    arg_kernels = [compose_rhs_kernel(ctx, ex.args[ia], arg_infos[ia], i, j) for ia in 1 : na]
    fun_call(ex.fun, arg_kernels...)
end


##########################################################################
#
#   main body compilation
#
##########################################################################

# Scalar mode (the result is sured to be a scalar)

function compile(ctx::ScalarContext, mode::ScalarMode, ex::TAssign)
    lhs = ex.lhs
    rhs = ex.rhs
    @assert isa(lhs, TVar) || isa(lhs, TScalarVar)# || isa(lhs, TScalarRef1)
    assignment(ju_expr(lhs), ju_expr(rhs))
end


# Element-wise (1D or Scalar)

function compile(ctx::ScalarContext, mode::EWiseMode{0}, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs

    # lhs must be TVar, otherwise mode would be either EWiseMode{1} or EWiseMode{2}
    @assert isa(lhs, TVar)

    # setup rhs

    (rhs_init, rhs_siz, rhs_ty, rhs_final, rhs_info) = setup_rhs(ctx, rhs)

    @gensym siz ty len i
    kernel = compose_kernel(ctx, ex, nothing, rhs_info, i)

    # integrate

    flatten_code_block(
        rhs_init,
        assignment(siz, rhs_siz),
        if_statement( :($siz == ()),
            code_block( # scalar
                assignment(lhs.name, ju_expr(rhs))
            ),
            flatten_code_block( # ewise 1D
                assignment(ty, rhs_ty),
                assignment(lhs.name, fun_call(:Array, ty, siz)),
                assignment(len, fun_call(:length, lhs.name)),
                for_statement(i, 1, len, kernel),
                rhs_final
            )
        )
    )
end


# Element-wise 1D

function compile(ctx::ScalarContext, mode::EWiseMode{1}, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs

    @assert(isa(lhs, TVar) || isa(lhs, TRef))

    # setup rhs

    (rhs_init, rhs_siz, rhs_ty, rhs_final, rhs_info) = setup_rhs(ctx, rhs)

    # setup lhs

    if isa(lhs, TVar)
        siz = gensym("siz")
        ty = gensym("ty")

        init_lhs = code_block(
            assignment(siz, rhs_siz),
            assignment(ty, rhs_ty),
            if_statement( :($siz == (1,)), 
                assignment(lhs.name, fun_call(:zero, ty)),
                assignment(lhs.name, fun_call(:Array, ty, siz)))
        )

        lhs_pre = init_lhs
        lhs_info = nothing
    else
        (lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
    end

    len = gensym("len")
    get_len = assignment(len, length_getter(ctx, lhs, lhs_info))

    # kernel & main loop

    i = gensym("i")
    kernel = compose_kernel(ctx, ex, lhs_info, rhs_info, i)

    main_loop = for_statement(i, 1, len, kernel)

    # integrate

    flatten_code_block(
        rhs_init,
        lhs_pre,
        get_len,
        main_loop,
        rhs_final)
end


# Element-wise 2D

function compile(ctx::ScalarContext, mode::EWiseMode{2}, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs

    @assert(isa(lhs, TVar) || isa(lhs, TRef))

    # setup rhs

    (rhs_init, rhs_siz, rhs_ty, rhs_final, rhs_info) = setup_rhs(ctx, rhs)

    # setup lhs

    if isa(lhs, TVar)
        ty = gensym("ty")
        siz = gensym("siz")
        init_lhs = code_block(
            assignment(siz, rhs_siz),
            assignment(ty, rhs_ty),
            assignment(lhs.name, fun_call(:Array, ty, siz))
        )

        lhs_pre = init_lhs
        lhs_info = nothing
    else
        (lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
    end

    m = gensym("m")
    n = gensym("n")
    get_mn = Expr(:(=), :( ($m, $n) ), size2d_getter(ctx, lhs, lhs_info))

    # kernel & main loop

    i = gensym("i")
    j = gensym("j")
    kernel = compose_kernel(ctx, ex, lhs_info, rhs_info, i, j)

    main_loop = for_statement(j, 1, n, for_statement(i, 1, m, kernel))

    # integrate

    flatten_code_block(
        rhs_init,
        lhs_pre,
        get_mn,
        main_loop,
        rhs_final)
end



##########################################################################
#
#   reduction composition & compilation
#
##########################################################################

function setup_reduc(ctx::ScalarContext, r::Union(TReduc, TColwiseReduc, TRowwiseReduc),
    ty::Symbol, siz::Symbol, s::Symbol, rlen::Symbol)

    init_args, infer_siz, infer_ty, final_args, args_info = setup_args(ctx, r.fun, r.args)

    tf = TFun{r.fun}()

    pre = flatten_code_block(
        init_args,
        assignment(siz, infer_siz),
        assignment(ty, infer_ty)
    )

    rinit = reduc_initializer(tf, ty)
    rempty = reduc_emptyval(tf, ty)
    rget = reduc_result_getter(tf, s, rlen)
    (pre, final_args, rinit, rempty, rget, args_info)
end

function compose_fold_kernel(ctx::ScalarContext, f::Symbol, s::Symbol, args, args_info, idxspec...)
    na = length(args)
    @assert na == 1 || na == 2
    tf = TFun{f}()

    if na == 1
        x1 = gensym("x1")
        rupdate = reduc_updater(tf, s, x1)
        a1_kernel = compose_rhs_kernel(ctx, args[1], args_info[1], idxspec...)

        flatten_code_block(
            assignment(x1, a1_kernel),
            rupdate
        )
    else
        x1 = gensym("x1")
        x2 = gensym("x2")
        rupdate = reduc_updater(tf, s, x1, x2)

        a1_kernel = compose_rhs_kernel(ctx, args[1], args_info[1], idxspec...)
        a2_kernel = compose_rhs_kernel(ctx, args[2], args_info[2], idxspec...)

        flatten_code_block(
            assignment(x1, a1_kernel),
            assignment(x2, a2_kernel),
            rupdate
        )
    end
end


function compose_fold_loop(ctx::ScalarContext, rfun::Symbol, s::Symbol, len::Symbol, args, args_info)

    @gensym i
    kernel = compose_fold_kernel(ctx, rfun, s, args, args_info, i)
    for_statement(i, 1, len, kernel)
end

function compose_fold_loop2(ctx::ScalarContext, rfun::Symbol, s::Symbol, m::Symbol, n::Symbol, args, args_info)

    @gensym i j
    kernel = compose_fold_kernel(ctx, rfun, s, args, args_info, i, j)
    for_statement(j, 1, n, for_statement(i, 1, m, kernel))
end


function compile(ctx::ScalarContext, mode::ReducMode, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs
    @assert isa(lhs, TVar) || isa(lhs, TScalarVar)
    @assert isa(rhs, TReduc)

    # generate symbols
    @gensym rlen siz ty
    s = lhs.name # compiler has ensured no alias between lhs and rhs

    # setup
    (pre, final_args, rinit, rempty, rget, args_info) = setup_reduc(ctx, rhs, ty, siz, s, rlen)

    if rget == s
        rpost = nothing
    else
        rpost = assignment(s, rget)
    end

    # main loop
    amode = rhs.arg_mode
    if isa(amode, EWiseMode{0}) || isa(amode, EWiseMode{1})
        main = compose_fold_loop(ctx, rhs.fun, s, rlen, rhs.args, args_info)
    else
        @assert isa(amode, EWiseMode{2})
        m = gensym("m")
        n = gensym("n")
        main_loop = compose_fold_loop2(ctx, rhs.fun, s, m, n, rhs.args, args_info)
        main = code_block(
            :( ($m, $n) = ($siz) ),
            main_loop
        )
    end

    # integrate

    flatten_code_block(
        pre,
        assignment(rlen, fun_call(qname(:to_length), siz)),
        if_statement( :( $rlen > 0 ),
            flatten_code_block(  # not empty
                assignment(s, rinit),
                main,
                rpost
            ),
            flatten_code_block(  # empty
                assignment(s, rempty)
            )
        ),
        final_args
    )
end


# partial reduction

function compile(ctx::ScalarContext, mode::ColwiseReducMode, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs
    @assert isa(lhs, TVar) || isa(lhs, TRef)
    @assert isa(rhs, TColwiseReduc)

    # generate symbols
    @gensym siz ty m n i j s

    # setup rhs
    (pre, final_args, rinit, rempty, rget, args_info) = setup_reduc(ctx, rhs, ty, siz, s, m)

    if rget == s
        rpost = nothing
    else
        rpost = assignment(s, rget)
    end

    # setup lhs
    if isa(lhs, TVar)
        lhs_pre = assignment(lhs.name, fun_call(:Array, ty, 1, n))
        lhs_info = nothing
    else
        (lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
    end

    lhs_kernel = compose_lhs_kernel(ctx, lhs, lhs_info, j)

    # main loop
    kernel = compose_fold_kernel(ctx, rhs.fun, s, rhs.args, args_info, i, j)
    main_loop = for_statement(j, 1, n, flatten_code_block(
        assignment(s, rinit),
        for_statement(i, 1, m, kernel),
        rpost,
        assignment(lhs_kernel, s)
    ))

    # integrate

    flatten_code_block(
        pre,
        :( ($m, $n) = Devectorize.to_size2d($siz) ),
        lhs_pre,
        if_statement( :( $m > 0 ),
            flatten_code_block(  # not empty
                main_loop
            ),
            flatten_code_block(  # empty
                assignment(s, rempty),
                for_statement(j, 1, n, assignment(lhs_kernel, s))
            )
        ),
        final_args
    )
end


function compile(ctx::ScalarContext, mode::RowwiseReducMode, ex::TAssign)

    lhs = ex.lhs
    rhs = ex.rhs
    @assert isa(lhs, TVar) || isa(lhs, TRef)
    @assert isa(rhs, TRowwiseReduc)

    # generate symbols
    @gensym siz ty m n i j s

    # setup rhs
    (pre, final_args, rinit, rempty, rget, args_info) = setup_reduc(ctx, rhs, ty, siz, s, n)

    # setup lhs

    if isa(lhs, TVar)
        lhs_pre = assignment(lhs.name, fun_call(:Array, ty, m, 1))
        lhs_info = nothing
    else
        (lhs_pre, lhs_info) = setup_lhs(ctx, lhs)
    end
    lhs_kernel = compose_lhs_kernel(ctx, lhs, lhs_info, i)


    # loops

    kernel = compose_fold_kernel(ctx, rhs.fun, s, rhs.args, args_info, i, j)

    init_loop = for_statement(i, 1, m, flatten_code_block(
        assignment(s, rinit),
        kernel,
        assignment(lhs_kernel, s)
    ))

    main_loop = for_statement(j, 2, n,
        for_statement(i, 1, m, flatten_code_block(
            assignment(s, lhs_kernel),
            kernel,
            assignment(lhs_kernel, s)
        ))
    )

    if rget == s
        post_loop = nothing
    else
        post_loop = for_statement(i, 1, m, code_block(
            assignment(s, lhs_kernel),
            assignment(s, rget),
            assignment(lhs_kernel, s)
        ))
    end

    # integrate

    flatten_code_block(
        pre,
        :( ($m, $n) = Devectorize.to_size2d($siz) ),
        lhs_pre,
        if_statement( :( $m > 0 ),
            flatten_code_block(  # not empty
                assignment(j, 1),
                init_loop,
                main_loop,
                post_loop
            ),
            flatten_code_block(  # empty
                assignment(s, rempty),
                for_statement(i, 1, m, assignment(lhs_kernel, s))
            )
        ),
        final_args
    )
end

##########################################################################
#
#   code-generating macros
#
##########################################################################

function dump_devec(ex::Expr)
    println(compile(ScalarContext(), ex))
end

macro devec(assign_ex)
    esc(compile(ScalarContext(), assign_ex))
end

macro inspect_devec(assign_ex)
    let code__ = compile(ScalarContext(), assign_ex)
        println("$assign_ex ==>")
        println(code__)
        esc(code__)
    end
end

