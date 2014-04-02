# Getting started with Haskell

Install the Haskell Platform or cabal + ghc.

* Cabal
* ghc is the official Haskell compiler.

## Hello World

Simple.

```
main = putStrLn "Hello, world"
```

Put this in a file (`hello_world.hs`). Compile it with `ghc hello_world.hs`,
and run the executable.

## ghci

Interpreter for Haskell. Not quite a read-execute loop like other languages,
but it's useful.

# Bindings

* The `=` sign declares bindings.
* Local bindings with let
* Haskell will auto-insert semicolons by a layout rule.
* You can bind functions.

```
add arg1 arg2 = arg1 + arg2
five = add 2 3
```

* Tokens on the line are function arguments
* Associativity - use parentheses for compound expressions

# Haskell is a **pure** functional language.

* By functions, we mean mathematical functions.
    * No side effects
    * Deterministic - same result every time it is run with an input
* Variables are immutable.
    * `x = 5; x = 6` is an error, since `x` cannot be changed.
* order-independent
* lazy - definitions of symbols are evaluated only when needed. If you divide
  two variables, for instance, it will not be evaluated until you read the
  result
    * This means you can divide by 0, create infinite lists... etc. so long as
      you're careful that those don't get evaluated.
* recursive - bound symbol is in scope within its own definition.

This program will cause an infinite loop (the program "diverges"), because the
variable `x` in `main` is defined in terms of itself, not in terms of the
declaration `x = 5`:

```
x = 5

main = let x = x + 1
        in print x
```

# How can you program without mutable variables?

## Loops

* In C, you use mutable variables to create loops (like a for loop).
* In Haskell, you can use recursion to "re-bind" argument symbols in a new scope
    (call the function with different arguments to get different behavior).
    * *Problem*: The example recursive factorial implementation in Haskell uses
     function calls to loop, but those function calls will create stack frames,
     which will cause Haskell to consume memory.
    * *Solution*: Haskell supports optimized **tail recursion**. Use an
      accumulator argument to make the factorial call tail recursive.

## Guards and where clauses

* *Guards* let you shorten function declarations by declaring conditions in
  which a function occurs:
    * Pipe ("`|`") symbol introduces a guard. Guards are evaluated top to
      bottom
    * the first `True` guard wins.
    * `otherwise` in the Haskell system `Prelude` evaluates to `true`
* Bindings can end with `where` clauses
    * Where clauses can scope over multiple guards
    * Convenient for binding variables to use in guards

## Variable names
* It's conventional in Haskell to have versions of variables and functions
  denoted by apostrophes ('). But David Mazieres finds that this can cause
  difficult to find bugs, so he suggests that you **use the longer symbol name
  for the larger scope**.

# Types

## Every expression and binding has a type (it is *strongly typed*)

<List of types>

* You can declare the type of a symbol or expression with double colons
  ("`::`").
    * The `::` operator has the lowest precedence, so you need to parenthesize.

## Haskell uses function *currying*.
* Functions are called one argument at a time.
* Consider `add 2 3`.
    * This is equivalent to `(add 2) 3`
    * `(add 2)` returns a function which takes one parameter - the second
      parameter in adding something.
* Usually, the compiler can infer types, using `:t`
    * It's a good idea to declare types of top-level bindings.

## Defining data types

Types start with capital letters.

* The `data` keyword
    * Give it a name
    * Give it a set of constructors
    * Tell what other types it derives from (`deriving Show` allows it to print
      your type, for example)
Example:

```
data PointT = PointC Double Double deriving show
```

* Types and constructors can use the same name, since they are in different
  namespaces.
    * But, you can have multiple constructors by declaring them with different
      names.
    * Constructors additionally don't need to take arguments

* Constructors act like functions producing values of their types.
* How to access these values?
    * *`case` statements*.
        * Example in slides
    * Spelling out the type in a function declaration through *pattern matching*
        * Example in slides

* Types can have parameters.
    * Some useful, parameterized types: `Maybe` and `Either`.
* You can deconstruct types and bind variables within guards. Example in
  slides.

# Lists

So common that Haskell has Lists as a predefined type with syntactic sugar.
Strings are just lists of `Char`s.

* Bullets from slides

## Constructors

Two constructors: `x:rest` and `[]`.

* `[]` is the empty list
* `x:rest` is an infix constructor of a variable to be prepended to the head of
  the `rest` of the list.

### Note on error code:

* `error` is a function of any type that throws an exception. It is intended for
  progamming errors that should never occur.

## Other methods

* The `++` infix operator is used for concatenation of lists: `list1 ++ list2`
* length
* filter

# Parsing with `deriving Read` and `reads`
* Unfortunately, parsing is more complicated than printing, since the string
  for an object may not parse correctly or may even be ambiguous.
* `reads` parses and returns a parsed object, along with the rest of the
  string.

# Useful tool: Hoogle

A search engine for Haskell and the Haskell libraries. David Mazieres
recommends that you make this a keyword in your search bar! Haskell may have a
steep learning curve, but it is relatively easy to understand what code is
doing (with tools like this).

# Example: counting letters
Due to **thunks** you don't actually have to keep an intermediate list in
memory at any point in time (see example in slides)

# Function composition

* The `.` infix operator provides function composition: `(f . g) x = f (g x)`.
* The new version doesn't name the argument, which is called **point-free**
  programming.
* This allows you to apply arguments kind of like right-to-left Unix piping.

# Lambda Extraction

* Sometimes you want to name arguments but not the function, which you can
  through **lambdas**.
* Use backslash ("`\\`") to declare a lambda.

# Infix vs Prefix notation

* Any type constructor can be written as a prefix or an infix.
    * If it starts with a lowercase letter, it's a prefix invocation
    * If it is surrounded by backticks ("`\``"), it's infix.
    * Example: `add 1 2 == 1 \`add\` 2`.
* If you don't add an argument, you're creating a function that is missing an
  argument (which can be applied to a new "first argument"
