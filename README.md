# Yip
Yip is a small programming being developed for my Compiler Construction module.

The Yip Comiler uses OCamllex and Menhir.

# Building
Make sure that you have OCaml and Menhir installed before attempting to build.

It has been tested using OCaml 4.02.1 and Menhir 20150912.

To build using the Makefile you can run:

```
make
```
To parse a file into an AST, run:
```
./main.native <filename>
```

# Testing
The file Test.ml contains some basic tests for the parser. To execute these tests, run:
```
./test.native
```

This will run each test then report the total results of all the tests.

There is also an expression test, comparing the output of arithmetic in OCaml and Yip:
```
./runtime.sh
```

# The Language
Yip is mainly based on elements from Lua and Haskell, mixing Lua with some functional elements borrowed from Haskell.

Variables are a handy part of the language. At the moment the types are functions and integers. There is no need to specify types as they are deduced by the compiler.

Integers are the main data type right now. You can perform arithmetic with integers and variables:
```
num = 9 * 2 + (18 - 5) / 3
res = num / 3
```

Newline characters ignored, thus the following two programs are equivalent:
```
x = 2
y = 5
z = "hi"
```

```
x = 2 y = 5 z = "hi"
```

Anonymous functions are also a part of the language. They are declared as:
```
fun <args> -> <code>
```

Here are some examples:
```
fun x -> x + 1
fun x -> -x
fun x y -> x + y
```

Anonymous functions can also be assigned to variables:
```
inc = fun x -> x + 1
neg = fun x -> -x
add = fun x y -> x + y
```

# The Main Function
Every program starts off running the main function. The main function returns an exit code, which can be used to display a result.

```
main = fun -> 2 + 2
```

# Function Applicatoin

You can apply a function using a C-like syntax:

```
add = fun x y -> x + y
main = fun -> add(1, 2)
```

# Blocks

Every function executes a single expression. This isn't very useful when you want to perform more complex operations.
Blocks are used to perform multiple expressions.

```
main = fun -> {
    a = 2 + 2
    b = a * 4
    c = a * b / 3
    c - 18
}
```

Each expression in the block is executed sequentially and the result from the final block is returned.


# Optimisation
The compiler currently implements constant folding in order to compute simple arithmetic.
Mathematics involving literal integers is computed at compile-time.

Here's an example of it working on a simple arithmetic expression
```
    x = 2 * (3 + 4) - 7
=>  x = 2 * 7 - 7
=>  x = 14 - 7
=>  x = 7
```
