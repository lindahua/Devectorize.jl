# include all unit testing files

tests = ["texpr", 
         "tmode", 
         "scalar_ref", 
         "scalar_ewise",
         "scalar_special",
         "scalar_reduc",
         "scalar_hybrid"]

# extensions

println("Testing Devectorize.jl")
println("---------------------------")

for t in tests
    fp = joinpath("test", "$(t).jl")
    println("* running $(fp) ...")
    include(fp)
end

