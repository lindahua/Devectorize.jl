# Benchmarks to compare the performance of 
#
#	- devec macro (in DeExpr)
#	- hand-coded devectorized for-loop
#	- vectorized Julia code
#

using DeExpr

# benchmark helpers

repeat = 20

macro my_bench(Name)	
	quote
		# create task
		println("bench: ", $(string(Name)))
		task = ($Name)()

		vec_eval(task, a, b, c)
		t1 = @elapsed for i = 1 : repeat
			vec_eval(task, a, b, c)
		end
		@printf "    vec_eval   : %8.4f sec  |  gain = %7.4f\n" t1 t1 / t1

		devec_eval(task, a, b, c)
		t2 = @elapsed for i = 1 : repeat
			devec_eval(task, a, b, c)
		end
		@printf "    devec_eval : %8.4f sec  |  gain = %7.4f\n" t2 t1 / t2

		hand_loop(task, a, b, c)
		t3 = @elapsed for i = 1 : repeat
			hand_loop(task, a, b, c)
		end
		@printf "    hand_loop  : %8.4f sec  |  gain = %7.4f\n" t3 t1 / t3

		println()
	end
end

# prepare data

m = 1000
n = 1000
a = zeros(m, n)
b = zeros(m, n)
c = zeros(m, n)


# simple element-wise

type simple_ewise end
formula(::simple_ewise) = "(a - b).^2 + c"

function vec_eval{T<:Real}(::simple_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	r = sqr(a - b) + c   # Note: DeExpr defines a sqr on arrays
end

function devec_eval{T<:Real}(::simple_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = sqr(a - b) + c
end

function hand_loop{T<:Real}(::simple_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	r = similar(a)
	for i = 1 : length(a)
		v = (a[i] - b[i])
		r[i] = v * v + c[i]
	end
end


# complex element-wise

type complex_ewise end
formula(::complex_ewise) = "log(exp((a - b).^2) + exp(a + b)) - c .* log(c)"

function vec_eval{T<:Real}(::complex_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	r = log(exp((a - b).^2) + exp(a + b)) - c .* log(c)
end

function devec_eval{T<:Real}(::complex_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = log(exp((a - b).^2) + exp(a + b)) - c .* log(c)
end

function hand_loop{T<:Real}(::complex_ewise, a::Array{T}, b::Array{T}, c::Array{T})
	r = similar(a)
	for i = 1 : length(a)
		r[i] = log(exp((a[i] - b[i]).^2) + exp(a[i] + b[i])) - c[i] .* log(c[i])
	end
end

# shift dot

type shift_dot end
formula(::shift_dot) = "sum( (a - mean(a)) .* (b - mean(b)) )"

function vec_eval{T<:Real}(::shift_dot, a::Array{T}, b::Array{T}, c::Array{T})
	r = sum( (a - mean(a)) .* (b - mean(b)) )
end

function devec_eval{T<:Real}(::shift_dot, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = sum( (a - mean(a)) .* (b - mean(b)) )
end

function hand_loop{T<:Real}(::shift_dot, a::Array{T}, b::Array{T}, c::Array{T})
	n = length(a)

	# calculate mean(a)
	sa = 0.
	for i = 1 : n
		sa += a[i]
	end
	ma = sa / n

	# calculate mean(b)
	sb = 0.
	for i = 1 : n
		sb += b[i]
	end
	mb = sb / n

	# calculate shift dot
	r = 0.
	for i = 1 : n
		r += (a[i] - ma) * (b[i] - mb)
	end
end

# column wise sum

type colwise_sum end
formula(::colwise_sum) = "sum(a, 1)"

function vec_eval{T<:Real}(::colwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	r = sum(a, 1)
end

function devec_eval{T<:Real}(::colwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = sum(a, 1)
end

function hand_loop{T<:Real}(::colwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	m, n = size(a)
	r = zeros(1, n)
	for j = 1 : n
		s = 0.
		for i = 1 : m
			s += a[i, j]
		end
		r[j] = s
	end
end

# row-wise sum

type rowwise_sum end
formula(::rowwise_sum) = "sum(a, 2)"

function vec_eval{T<:Real}(::rowwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	r = sum(a, 2)
end

function devec_eval{T<:Real}(::rowwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = sum(a, 2)
end

function hand_loop{T<:Real}(::rowwise_sum, a::Array{T}, b::Array{T}, c::Array{T})
	m, n = size(a)
	r = zeros(1, n)
	for j = 1 : n
		for i = 1 : m
			r[i] += a[i,j]
		end
	end
end


# col-wise euclidean

type colwise_eucdist end
formula(::colwise_eucdist) = "sqrt(sum((a - b).^2, 1))"

function vec_eval{T<:Real}(::colwise_eucdist, a::Array{T}, b::Array{T}, c::Array{T})
	r = sqrt(sum(sqr(a - b), 1))
end

function devec_eval{T<:Real}(::colwise_eucdist, a::Array{T}, b::Array{T}, c::Array{T})
	@devec r = sqrt(sum(sqr(a - b), 1))
end

function hand_loop{T<:Real}(::colwise_eucdist, a::Array{T}, b::Array{T}, c::Array{T})
	m, n = size(a)
	r = zeros(1, n)
	for j = 1 : n
		s = 0.
		for i = 1 : m
			v = a[i, j] - b[i, j]
			s += v * v
		end
		r[j] = sqrt(s)
	end
end


#  Benchmarks

@my_bench simple_ewise
@my_bench complex_ewise
@my_bench shift_dot
@my_bench colwise_sum
@my_bench rowwise_sum
@my_bench colwise_eucdist


