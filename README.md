## DeExpr -- A Julia Framework for Delayed Expression Evaluation

In many programming languages (including Julia), expressions are immediately evaluated upon construction. This simple strategy often results in less than optimal behaviors, which, for example, include *creation of unnecessary temporaries* and *repeated memory round-trips*. Consider the following example,

```julia
r = a .* b + c .* d + a
```

With immediate evaluation, three temporaries, respectively for storing the results of ``a .* b``, ``c .*d ``, and ``a .* b + c .* d``. Also, the array ``a`` will be traversed twice. Moreover, computation on large arrays is often memory-bound -- the run-time performance largely depends on how many times you have to scan the arrays. 

For the formula above, a much more efficient way to evaluate it can be expressed using for-loops as follows

```julia
n = length(a)
r = zeros(n)
for i = 1 : length(a)
	r[i] = a[i] * b[i] + c[i] * d[i] + a[i]
end
```

With this piece of code, you can get all the results in one pass, without creating any temporary arrays.
However, low-level for-loops are often much longer and more difficult to read, write, and maintain. 

> Is it possible to combine the elegance of high-level expressions and the performance of low-level for-loops?

The answer is *Yes*. A popular approach to this goal is **delayed evaluation** (often called *lazy evaluation*) -- the basic idea is to delay the evaluation until the results are needed. Let's look at the examples above, we can hold off the evaluation of all the temporaries until the assignment to ``r`` happens -- at this point, an integrated loop is emitted to compute all results in one pass.

The powerful meta-programming framework of Julia makes it possible to deliver this goal using incredibly simple syntax. Taking advantage of this framework, *DeExpr* provides a macro ``@devec``:

```julia
@devec r = a .* b + c .* d + a
``` 

This statement is exactly the same as the one we saw above -- except for the macro ``@devec``, which performs all the magic of translating the formula into a one-pass loop behind the scenes.


