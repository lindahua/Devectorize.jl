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

r = zeros(size(a))

# generate codes to evaluate the expression in de-vectorized way
@devec r = a + b * sin(c) / 2 + exp(a + b)

# generate SIMD codes to evaluate the expression
@de_simd r = a + b * sin(c) / 2 + exp(a + b)

```
