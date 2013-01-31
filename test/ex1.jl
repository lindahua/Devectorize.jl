
using DeExpr


#e = de_wrap(:(a + (2 * b)))

n = 1000000
a = rand(n)
b = rand(n)
c = rand(n)
r = zeros(n)

ctx = ScalarContext()
gen = de_compile(ctx, :(r = a + sin(a + b) .* exp(a - c)) )
println("generated de-vectorized code")
println("---------------------------------")
println(gen)
println()

println("Benchmarks")
println("---------------------------------")

macro my_bench(FName)	
	quote
		println("bench: ", $(string(FName)))
		local t0 = @elapsed ($FName)(a, b, c, r)  # warming
		local repeat = 10
		local t1 = @elapsed for i = 1 : repeat
			($FName)(a, b, c, r)
		end
		println("    initial run = $t0 sec")
		println("    average run = $(t1 / repeat) sec")
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
