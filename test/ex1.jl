
using DeExpr

#e = de_wrap(:(a + (2 * b)))

n = 1000000
a = rand(n)
b = rand(n)
c = rand(n)
r = zeros(n)

ctx = ScalarContext()
gen = de_generate(ctx, :(r = a + sin(a + b) .* exp(a - c)) )
println("generated de-vectorized code")
println("---------------------------------")
println(gen)
println()

println("Benchmarks")
println("---------------------------------")

macro my_bench(FName)	
	quote
		println("bench: ", $(string(FName)))
		($FName)(a, b, c, r)  # warming
		@time for i = 1 : 10
			($FName)(a, b, c, r)
		end
	end
end

function use_rawloop{T<:Real}(a::Vector{T}, b::Vector{T}, c::Vector{T}, r::Vector{T})
	i = length(r)
	for i = 1 : n
		r[i] = a[i] + sin(a[i] + b[i]) * exp(a[i] - c[i])
	end
end

function use_vectorized{T<:Real}(a::Vector{T}, b::Vector{T}, c::Vector{T}, r::Vector{T})
	r = a + sin(a + b) .* exp(a - c)
end

function use_devec{T<:Real}(a::Vector{T}, b::Vector{T}, c::Vector{T}, r::Vector{T})
	@devec r = a + sin(a + b) .* exp(a - c)
end

@my_bench use_rawloop
@my_bench use_vectorized
@my_bench use_devec

