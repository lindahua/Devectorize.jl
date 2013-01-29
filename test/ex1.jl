
require("../src/de_eval_base.jl")

e = de_wrap(:(1 + (2 .* x)))

println(e)
