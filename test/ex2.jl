
using DeExpr

#e = de_wrap(:(a + (2 * b)))

n = 1000000
a = rand(n)
b = rand(n)
c = rand(n)
r = zeros(n)

ex = :( sum(a + sin(a + b) .* exp(a - c)) )
println("Element-wise: $ex")

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

function use_rawloop{T<:Real}(a::Array{T}, b::Array{T}, c::Array{T}, r::Array{T})
	s = zero(T)
	for i = 1 : n
		s += (a[i] + sin(a[i] + b[i]) * exp(a[i] - c[i]))
	end
	return s
end

function use_vectorized{T<:Real}(a::Array{T}, b::Array{T}, c::Array{T}, r::Array{T})
	s = sum(a + sin(a + b) .* exp(a - c))
end

function use_devec{T<:Real}(a::Array{T}, b::Array{T}, c::Array{T}, r::Array{T})
	@devec s = sum(a + sin(a + b) .* exp(a - c))
end

@my_bench use_rawloop
@my_bench use_vectorized
@my_bench use_devec

