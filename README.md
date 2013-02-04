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

The answer is *Yes*. A popular approach to this goal is **delayed evaluation** (often called *lazy evaluation*) -- the basic idea is to delay the evaluation until the results are needed. Let's look at the examples above, we can hold off the evaluation of all the temporaries until the assignment to ``r`` happens -- at this point, an integrated loop is emitted to compute all results in one pass.

The powerful meta-programming framework of Julia makes it possible to deliver this goal using incredibly simple syntax. Taking advantage of this framework, *DeExpr* provides a macro ``@devec``:

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

Here, we use vectorized Julia code as the baseline, and report the performance gain (for example, if the baseline takes 1 sec, and devec takes 0.5sec, then the gain is 2x). We can see that codes marked with ``@devec`` macro typically performs 2x to 5x faster than vectorized codes, and is comparable (sometimes even slightly faster than) a hand-coded for loop. 

You can run the script ``test/bench_devec.jl`` that comes with the *DeExpr* package to test the benchmark on your machine.

It is important to note that *DeExpr* only recognizes a subset of expressions of Julia (but the most commonly used subset), as listed below.


 













