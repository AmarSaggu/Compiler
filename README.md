# Compiler
A compiler for a small language being developed for the Compiler Construction module using OCamllex and Menhir.

# Building
Make sure that you have OCaml and Menhir installed before attempting to build.

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

# The Language
The language being created here will be based on elements from Lua and Haskell, mixing Lua with some functional elements borrowed from Haskell.

There are two types of variables in the language right now; integers and strings. There is no need to specify types as they are deduced by the compiler.

A program just consists of a list of variable assignments. The following is be a valid program:
```
a = 81
b = "hi"
```

Integers are the main data type right now. You can perform arithmetic with integers and variables:
```
num = 9 * 2 + (18 - 5) / 3
res = num / 3
```

Strings can be declared, but currently there are no string operations:
```
str = "Hello World!"
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
