# extensions from other sources

##########################################################################
#
#   @devec_transform - a code-generating macro for associative types
#
#   Note: this extension was contributed by Tom Short
#
##########################################################################
#
#   Starting with an associative type `d`, the following assigns or
#   replaces key `a` with the result of `x + y` where `x` or `y` could
#   be keys in `d`.
#
#       @devec_transform d  a = x + y
#
#   This basically converts to the following:
#
#       var1 = has(d, :x) ? d[:x] : x
#       var2 = has(d, :y) ? d[:y] : y
#       @devec res = var1 + var2
#       d[:a] = res
#
#   It contains machinery to convert the symbol to a key type
#   appropriate for the associative type. For example, DataFrames have
#   string keys, so the symbol from the expression needs to be
#   converted to a string. Also of issue is
#
#   The following forms are supported:
#
#       @devec_transform d  a = x + y  b = x + sum(y)
#
#       @devec_transform(d, a => x + y, b => x + sum(y))
#
#
##########################################################################


# The following is like Base.haskey, but converts symbols to appropriate
# key types.
xhas(d, key) = haskey(d, key)
xhas{K<:AbstractString,V}(d::Associative{K,V}, key) = haskey(d, string(key))

# The appropriate key for the type
bestkey(d, key) = key
bestkey{K<:AbstractString,V}(d::Associative{K,V}, key) = string(key)

#### The following will be needed in package DataFrames for support
#
#   xhas(d::AbstractDataFrame, key::Symbol) = haskey(d, string(key))
#   bestkey(d::AbstractDataFrame, key) = string(key)
#   bestkey(d::NamedArray, key) = string(key)
#

# This replaces symbols with gensym'd versions and updates
# a lookup dictionary.
replace_syms(x, lookup::Associative) = x

function replace_syms(s::Symbol, lookup::Associative)
    if haskey(lookup, s)
        lookup[s]
    else
        res = gensym("var")
        lookup[s] = res
        res
    end
end

function replace_syms(e::Expr, lookup::Associative)
    if e.head == :(=>)
        e.head = :(=)
    end
    if e.head == :call
        length(e.args) <= 1 ? Expr(e.head, e.args) : Expr(e.head, e.args[1], map(x -> replace_syms(x, lookup), e.args[2:end])...)
    elseif e.head == :comparison
        Expr(e.head, replace_syms(e.args[1], lookup), e.args[2], replace_syms(e.args[3], lookup))
    else
        isempty(e.args) ? Expr(e.head, e.args) : Expr(e.head, map(x -> replace_syms(x, lookup), e.args)...)
    end
end

function devec_transform_helper(d, args...)
    var_lookup = Dict()
    lhs_lookup = Dict()
    body = Any[]
    for ex in args
        push!(body, compile(ScalarContext(), replace_syms(ex, var_lookup)))
        lhs_lookup[ex.args[1]] = true
    end
    # header
    header = Any[]
    for (s,v) in var_lookup
        push!(header, :($v = Devectorize.xhas(d, Devectorize.bestkey(d, $(Meta.quot(s)))) ?
            d[Devectorize.bestkey(d, $(Meta.quot(s)))] : isdefined($(Meta.quot(s))) ? $s : nothing))
    end
    # trailer
    trailer = Any[]
    for (s,v) in lhs_lookup
        push!(trailer, :(d[Devectorize.bestkey(d, $(Meta.quot(s)))] = $(var_lookup[s])))
    end
    push!(trailer, :(d))
    esc(:(let d = $d; $(Expr(:block, header..., body..., trailer...)); end))
end

macro devec_transform(df, args...)
    devec_transform_helper(df, args...)
end

