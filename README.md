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
./test.native <filename>
```


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
let x = 2
let y = 5
let z = "hi"
```

```
let x = 2 let y = 5 let z = "hi"
```
