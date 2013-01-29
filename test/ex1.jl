
require("../src/de_eval_base.jl")

e = de_wrap(:(a + (2 * b)))

println(pretty(e))
