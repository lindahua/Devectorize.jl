## A Julia Framework for Delayed Expression Evaluation

The library provides functions and macros to map Julia expressions to different back-end implementations.

The key function is ``de_generate`` which takes an assignment expression and generates efficient codes to evaluate it depending on contexts (e.g. choice of back-ends).

```julia
expr = :( r = a + b * sin(c) / 2 + exp(a + b) )

# generate de-vectorized scalar for-loop 
code = de_generate( ScalarContext(), expr )  

# generate AVX SIMD loops
code = de_generate( SIMDContext{AVX}(), expr )

# you can then evaluate the code as
eval( code )

``` 

Macros are provided to simplify the coding

```julia

# generate codes to evaluate the expression in de-vectorized way
@devec r = a + b * sin(c) / 2 + exp(a + b)

# generate SIMD codes to evaluate the expression
@de_simd r = a + b * sin(c) / 2 + exp(a + b)

```

In general, the codes written as above will automatically infer the shape and size of r before performing the computation. If your left hand side has already been allocated, you can eliminate this overhead by writing the expressions as follows

```julia
r = zeros(size(a))
@devec r[:] = a + b .* c
``` 

In this case, the de-vectorized code will directly write results to the pre-allocated storage instead of creating a new array. 
