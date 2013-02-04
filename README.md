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
**Notes:** 

- Operator ``*`` and ``/`` are not supported, as they entail complex semantics depending on the arguments which may only be known at run-time. Users can use ``.*`` and ``./`` to express element-wise multiplication and division, which are perfectly supported in *DeExpr*. 

- These three functions: ``sqr``(x -> x * x), ``rcp``(x -> 1 / x), and ``blend``((c, x, y) -> c ? x : y) are not in the Base module of Julia. They are provided by *DeExpr* as extensions to make it easier to write vectorized expressions (and then ``@devec`` it).
 
### Simple references

```julia
@devec r = x[:,1] + y[:,2]
@devec r = a[i,:] .* b
@devec r[:,j] = x + sin(a[:,j])
```

Simple reference here means the reference expressions in either of the following forms: 
```a[:], a[i], a[i, j], a[:, :], a[:, i], a[i, :]``, where i can be either an integer or a symbol that refers to an integer variable. Reference expressions can appear in both left and right hand side of an assignment. Support of more flexible references is planned for future releases.

Note that when you write
```julia
r = a + b .* c
```
*DeExpr* will emit codes that creates an array to store the results and bound it to ``r``, this process may entails some overhead of inferring the type and shape of the result and creating a new array. When ``r`` has been created, you may eliminate such runtime overheads by writing
```
r[:] = a + b .* c
```
Then, the results will be directly written to ``r``, and no array will be created before evaluation.


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

### Block expressions

```julia
@devec begin
	a = sin(x) - cos(y)
	b = sum(a) + exp(z)
	c = x .* y - max(b)
end
```

In current implementation, *DeExpr* simply de-vectorizes each assignment expression respectively. In future version, it may use a more sophisticated algorithm to identify opportunities of sharing some computation across expressions.


## Design of the Framework

In *DeExpr*, the process of translating a Julia expression into de-vectorized codes goes through two stages:

- translate a Julia expression to a typed expression (enriched with semantic information), using ``texpr``
- compile the typed expression into de-vectorized codes, using ``compile``, which itself takes three steps:

	- decompose a given expression into a sequence of basic expressions (e.g. break a hybrid expression or a block expression)
	- compose loops for each basic expression via a *back-end* factory, using ``compose``
	- integrate all generated loops into a code block and return
	
### Typed expressions

Julia front-end parses any input expression into an instance of ``Expr``, which contains only syntatic information but not semantic information. To generate the code, one has to first understand the *semantics* (i.e. *meaning*) of the expression, e.g. whether it is doing a reduction or a element-wise transformation.

To express the *semantics* of an expression, *DeExpr* establishes a type hierarchy in ``src/texpr.jl``. The hierarchy can be briefly summarized as follows

	TExpr
	-- TEWise  	   # everything can serve as an element-wise argument
	   -- TScalar  # everything that is sure to be a scalar 
	      -- TNum  # numerical literals, e.g. 1, 2.0, ...
	      -- TScalarSym     # a symbol that is known to be a scalar (e.g. result of a full reduction)
	      -- TRefScalar1    # a[i]
	      -- TRefScalar2    # a[i,j]
	   -- TSym     # a symbol that refers to a variable (can be an array or a scalar)
	   -- TRef
	      -- TRef1D     # a[:]
	      -- TRef2D     # a[:,:]
	      -- TRefCol    # a[:,j]
	      -- TRefRow    # a[i,:]
	   -- TMap     # element-wise map, e.g. sin(a), a + b, a + b .* c, ...
	-- TReduc           # full reduction, e.g. sum(a), sum(a + b .* c), ...
	-- TColwiseReduc    # column-wise reduction
	-- TRowwiseReduc    # row-wise reduction
	-- TAssign     # asssignment, e.g. a = sin(x), r[:,i] = a + cos(x[:,j]), ...
	-- TBlock      # a block of expressions
	
The function ``texpr`` (also defined in ``src/texpr.jl``) takes an instance of ``Expr`` as an argument, analyzes it. If the expression is recognized, it returns a typed expression (i.e. an instance of ``TExpr``), otherwise, it raises an error (to be more specific, throws an exception of type ``DeError``.)

The analysis performed in ``texpr`` relies on the semantic information provided by the functions in ``src/fun_traits.jl``. These functions can tell you ``sin`` is an element-wise mapping that takes one argument, while ``sum`` is a reduction. They also tell you result type information, e.g. the element type of ``a + b`` is ``promote_type(eltype(a), eltype(b))``, while that for ``.==`` is ``Bool``.


### Contexts

To make the framework extensible, *DeExpr* introduces the notion of *Context*, which refers to a specific setting in which the codes are generated (e.g. CPU, SIMD, CUDA, OpenCL, etc)

The abstract type *EvalContext* (in ``src/compile_base.jl``) is used as the super class of all contexts. In current version, *DeExpr* provides a specific context type, namely ``ScalarContext``, in which expressions are mapped to de-vectorized for-loops. 

In future, other contexts might be introduced (e.g. SIMD and CUDA), thus providing users options to choose specific ways to emit the evaluation code for their expressions.

### Compilation

The function ``compile`` takes two arguments: a context and a typed expression, and returns a the generated codes. Generally, this function is a driver, which actually delegates the code generation to two functions: ``compose_init`` (for generating codes for initialization) and ``compose_main`` (for generating the main loops). These two functions are provided by specific back-ends.

To reduce the complexity of back-end implementation, the ``compile`` function performs some preprocessing, which includes

- translates blocks and hybrid expressions into a sequence of basic expressions
- identifies trivial assignments (i.e. ``a = b``), and simply emits it (as ``a = b``). Note that this simply bounds the name ``a`` to the object referred by ``b``, which does not involve any *real computation*.
- take precautions to prevent potential alias problems. For example, it translates ``a = b + sin(a)`` into two statements, ``tmp = b + sin(a)``, and a trivial assignment ``a = tmp``. The temporary name is generated using ``gensym`` to avoid collision with other names.

After this processing, the back-end can be implemented in a much simpler way, without taking into account such intricacies.

The functions to generate codes for ``ScalarContext`` are in ``src/scalar_backend.jl``. 

### Code composition

The routines in ``src/scalar_backend.jl`` uses *recursive kernel composition* to generate loop kernels.

Take the expression ``a + b .* c`` for example. It first generates ``get_value(a, i)``, ``get_value(b, i)``, and ``get_value(c, i)`` for the terminals ``a``, ``b``, and ``c``. Here, ``get_value`` is an overloaded function to ensure correct behavior for different cases (e.g. ``a`` can be either a scalar or an array). 

For ``b .* c``, it takes the generated kernel for ``b`` and ``c`` (as above), combines them with the operator ``.*``, and then emits ``.*(get_value(b, i), get_value(c, i)``. Likewise, for ``a + b .* c``, it emits ``+(get_value(a, i), .*(get_value(b, i), get_value(c, i))``.

The ``compose_main`` function will generate a loop that uses the generated kernel as the loop body.


