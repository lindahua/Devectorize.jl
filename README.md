## DeExpr -- A Julia Framework for De-vectorized Evaluation

In many programming languages (including Julia), expressions are immediately evaluated upon construction. This simple strategy often results in less than optimal behaviors, which, for example, include *creation of unnecessary temporaries* and *repeated memory round-trips*. Consider the following example,

```julia
r = a .* b + c .* d + a
```

With immediate evaluation, three temporaries, respectively for storing the results of ``a .* b``, ``c .*d ``, and ``a .* b + c .* d``. Also, the array ``a`` will be traversed twice. Moreover, computation on large arrays is often memory-bound -- the run-time performance largely depends on how many times you have to scan the arrays. 

For the formula above, a much more efficient way to evaluate it can be expressed using for-loops as follows

```julia
n = length(a)
r = zeros(n)
for i = 1 : n
	r[i] = a[i] * b[i] + c[i] * d[i] + a[i]
end
```

With this piece of code, you can get all the results in one pass, without creating any temporary arrays.
However, low-level for-loops are often much longer and more difficult to read, write, and maintain. 

> Is it possible to combine the elegance of high-level expressions and the performance of low-level for-loops?

The answer is *Yes*. Let's look at the examples above, we can hold off the evaluation of all the temporaries until the assignment to ``r`` happens -- at this point, an integrated loop is emitted to compute all results in one pass.

The powerful meta-programming framework of Julia makes it possible to achieve this goal using incredibly simple syntax. Taking advantage of this framework, *DeExpr* provides a macro ``@devec``:

```julia
@devec r = a .* b + c .* d + a
``` 

This statement is exactly the same as the one we saw above -- except for the macro ``@devec``, which performs all the magic of translating the formula into a one-pass loop behind the scenes.

The remaining part is organized into two section: *Basic Usage*, which introduces how to use *DeExpr* to improve the performance of your code, and *Design of the Framework*, which provides a brief overview of the framework and its structures. 

## Basic Usage

For ordinary users, you only have to remember one macro -- ``@devec``. Putting it before the assignments that you want to *de-vectorize*, it will automatically translate your expressions into efficient loops.

Here is a table of benchmark results on some typical cases.

|                 |  julia vec |  @devec  | hand-coded loop |
| -------------   | -----------|---------|-----------------|
| simple-ewise    |   1.0000   | 2.5986x |  2.5869x |
| complex-ewise   |   1.0000   | 2.5934x |  2.5658x |
| shift-dot       |   1.0000   | 7.8894x |  7.0740x |
| colwise-sum     |   1.0000   | 1.2844x |  1.2894x |
| rowwise-sum     |   1.0000   | 4.2734x |  4.1988x |
| colwise-eucdist |   1.0000   | 5.9502x |  5.8356x |

*The result was obtained with Julia ``commit 3f92b13210 (2013-02-03)`` on Mac OS X 10.8, using the script ``test/bench_devec.jl``, which comes with the DeExpr package.*

Here, we use vectorized Julia code as the baseline, and report the performance gains (for example, if the baseline takes 1 sec, and devec takes 0.5sec, then the gain is 2x). We can see that codes tagged with the ``@devec`` macro typically performs 2x to 5x faster than vectorized codes, and is comparable (sometimes even slightly faster than) a hand-coded for loop. 

It is important to note that *DeExpr* only recognizes a subset of expressions of Julia (but the most commonly used subset), as listed below.

### Element-wise map of numbers and arrays

```julia
@devec r = a + b + c
@devec r = sin(a) + exp(a + 1.0) .* log(c)
```

Here is the list of operators and functions currently supported by *DeExpr*:

```julia
+, -, .+, .-, .*, ./, .^, max, min, clamp, blend,
.==, .!=, .<, .>, .<=, .>=, 
sqrt, cbrt, sqr, rcp, floor, ceil, round, trunc,
exp, log, log10, exp2, log2, expm1, log1p, 
sin, cos, tan, asin, acos, atan, 
sinh, cosh, tanh, asinh, acosh, atanh,
erf, erfc, gamma, lgamma, digamma
```
**Note:** These three functions: ``sqr``(x -> x * x), ``rcp``(x -> 1 / x), and ``blend``((c, x, y) -> c ? x : y) are not in the Base module of Julia. They are provided by *DeExpr* as extensions to make it easier to write vectorized expressions (and then ``@devec`` it).
 
### Simple references

```julia
@devec r = x[:,1] + y[:,2]
@devec r = a[i,:] .* b
@devec r[:,j] = x + sin(a[:,j])
```

Simple reference here means the reference expressions in either of the following forms: 
```a[:], a[i], a[i, j], a[:, :], a[:, i], a[i, :]``, where i can be either an integer or a symbol that refers to an integer variable. Reference expressions can appear in both left and right hand side of an assignment.

Support of more flexible references is planned for future releases.

### Op-assignment

```julia
@devec r += a
@devec r[:,i] .*= sin(a)
```

*DeExpr* will automatically translate them into ordinary assignment expressions.


### Full/Colwise/Rowwise reduction

```julia
@devec r = sum(a + b)
@devec r = max(sin(a), (), 1)
@devec r[:,j] = mean(a, 2)
``` 

*DeExpr* currently recognizes five reduction functions ``sum``, ``max``, ``min``, ``mean``, and ``dot``.


### Hybrid expressions

Consider the example below, 

```julia
@devec r = (a - sum(a)) .* b
```

This seemingly simple expression actually requires two loops to evaluate, one for computing ``sum(a)``, and the other for the top-level element-wise expression. *DeExpr* recognizes such situations, and will emit correct codes to perform the evaluation. For the example above, *DeExpr* will first break the expression into two ones, as 

```julia
@devec tmp1 = sum(a)
@devec r = (a - tmp1) .* b
```

Note that *DeExpr* only breaks expressions only when it is really necessary to do so, and tries to generate as few memory traversals as possible.




