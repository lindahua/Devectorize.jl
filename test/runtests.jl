tests = ["texpr",
         "tmode",
         "scalar_ref",
         "scalar_ewise",
         "scalar_special",
         "scalar_reduc",
         "scalar_hybrid",
         "extensions" ]

# extensions

println("Testing Devectorize.jl")
println("---------------------------")

for t in tests
    fp = "$(t).jl"
    println("* running $(fp) ...")
    include(fp)
end
