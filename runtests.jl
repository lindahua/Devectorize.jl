# include all unit testing files

tests = []

# extensions

println("Testing Devectorize.jl")
println("---------------------------")

for t in tests
    fp = joinpath("test", "$(t).jl")
    println("* running $(fp) ...")
    include(fp)
end

