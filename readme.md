# toy lang

This is a little system I put together to learn about Pratt parsers and implementing
simple functional programming languages.  I'm curious about Pratt parsers and setting up
a table with the parsing rules rather than the traditional way of capturing the parsing 
rules in recursive functions.  I think there's some potential in updating the parsing table
at parse by the program being parsed.

The source file `parse.ml` defines several types used in the tables, and a number of
helper functions to create values of those types.  The funciton `parse` drives the
parser using a table.  The table itself is not part of the parser code. Rather, it gets
passed in as part of the `pc` parameter.  The parse table itself is in the file `toylang.ml`
in the variable `pc`.

### the language

The toy language is a functional language inspired by OCaml in addition to being
implemented in it. Some differences include:
* the language is not type checked
* there are no:
  * strings
  * exceptions
  * modules
  * libraries
  * comments
  * floating-point numbers
  * user-defined types
  * pattern matching
* the keyword to create functions is `fn` rather than `fun`
* function definitions must have exactly 1 argument
* recursive functions are defined with `fix...and...in...` rather than `let
  rec...and...in...`
* there is no shorthand way of defining functions with `let`
* prefix `-` (negation) has the same precedence as multiplication
* the `else` clause in an `if...then...else...` expression is manditory
* there is no way to define infix operators
* simplistic tokenizer (see below)

### tokens in toy language

Identifiers are alphabetic characters only; no underscore or digits. The lexer understands
3 classes of characters: alphabetic; digits; and, printable symbols. Space is only
significant to separate tokens. The tokenizer will greedily consume a sequence of
characters of a class into one token.

The string `abc123` is interpreted as two tokens `abc` and `123`. The string `)))` will
become a single token of three closing parenthesis rather than three tokens of a single
parenthesis each. What you want is probably `) ) )`. The only multi-punctuation operators
in the language are `->`, `||`, `&&`, `<=`, `>=`, `**`, `@@`, and `!=`. Other sequences of
punctuation characters will probably cause unusual errors.

### building and running

Note: This repository only has the interesting parsing pieces and does not build.

One can use `ocamlbuild` to create an executable, like so

    ocamlbuild toylang.native

To run the REPL,

    ./toylang.native

### the cli

There is a simple command line interface with a primitive REPL. There is no support for
line editing except backspace. Any syntax or parsing error will cause the REPL to abort
with an exception message.

A `?` is the prompt. A `??` is the continuation prompt: the REPL has detected a partial
expression and is waiting for the remainder. If you want to abandon the partial
expression, enter an empty line.

`CTRL-C` to exit.

### code examples

    ? 2 + 3
    5

    ? -3**2
    -9

    ? let x = 2 in let y = 3 in y - x
    1

    ? let x = 2 and y = 3 in y - x
    1

    ? let x = 2 in if 1 = 1 then let y = 4 in x + y else 7
    6

    ? let x = 2 in if 1 = 2 then let y = 4 in x + y else 7
    7

    ? (fn a -> a + 3) 7
    10

    ? let a = (fn x -> fn y -> x + y + 1) in a 2 3
    6

    ? let a = (fn x -> fn y -> x + y + 1) in a 2 (2 + 1)
    6

    ? let a = (fn x -> x + 2) in a3
    5

    ? (let x = 2 in (fn y -> x + y) ) 3
    5

    ? fix even = fn x -> if x = 0 then true else odd (x - 1) and odd = fn x -> if x = 0 then false else even (x - 1) in even 88
    tis true

    ? fix even = fn x -> if x = 0 then true else odd (x - 1) and odd = fn x -> if x = 0 then false else even (x - 1) in even 89
    tis false
    
    ? fix a = (fn x -> x + 1) and b = (fn x -> x + 2) in a (b 3)
    6

    ? fix A = fn m -> fn n -> if m = 0 then n + 1 else if (m > 0) && (n = 0) then A (m - 1) 1 else A (m - 1) (A m (n - 1) ) in A 3 4
    125
    
A `let...` or `fix...` expression without an `in...` clause will expand the REPL's global
environment:

    ? let id = (fn x -> x)
    ? id 3
    3

    ? let a  = 3
    ? a
    3
    ? let a = a + 1 in a
    4
    ? a
    3

    ? fix even = fn x -> if x = 0 then true else odd (x - 1) and odd = fn x -> if x = 0 then false else even (x - 1)
    ? even 88
    tis true
    ? odd 89
    tis true

Entering only part of an expression with cause the REPL to prompt for the rest of it:

    ? let a = 3 in
    ?? let b = 4 in
    ?? a + b
    7
    ?
    
    ? let a = 3 in
    ?? 
    ? 


# Note
This code is originally from early 2018.  I've only recently added it to my GitHub.