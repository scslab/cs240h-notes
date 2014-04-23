% Basics
% Arun Debray

Lecture given by David Mazières on April 1, 2014.

This course is taught by David Mazières and Bryan O'Sullivan, who
together have done a lot of systems programming and research and
Haskell. Meanwhile, the CA, David Terei, is a member of the Haskell
language standards committee...

The goal of this class is to learn how to use Haskell to program systems
with reduced upfront cost. Haskell is typically taught in the context of
programming language research, but this course will adopt a systems
perspective. It's a surprisingly flexible and effective language, and
since it was created by and for programming language researchers, there
are lots of interesting things to do, and it's extremely flexible (if
you want to try something new, even if it's syntactical, this is as easy
as using a library, unlike most programming languages).

The first week of this class will cover the basics of Haskell, though
having some prior experience or a supplement (e.g. *[Real World
Haskell](http://book.realworldhaskell.org/)* or *[Learn You a
Haskell](http://learnyouahaskell.com/)*) is helpful. After the basics,
we will cover more advanced techniques. The class grade will be based on
attendance and participation, with scribing one of the lectures, and
also three warm-up programming assignments and a large final project and
presentation, in groups of one to three people.

Now, let's talk about Haskell.

In order to use Haskell, one will want to install [the Haskell platform](http://hackage.haskell.org/platform/)
or [`cabal`](http://www.haskell.org/cabal/users-guide/), along with the Haskell compiler, [`ghc`](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html). The simplest program
is

~~~ {.haskell}
main = putStrLn "Hello, world!"
~~~

Unsurprisingly, this is a "Hello, world!" program. One can compile it,
e.g. `ghc -o hello hello.hs`, but also load it into an interpreter
`ghci` (in this regard, Haskell is much like Lisp).

The first thing you've noticed is the equals sign, which makes a
binding, e.g.

~~~ {.haskell}
x = 2       -- Two hyphens introduce a comment.
y = 3       -- Comments go until the end of a line.
main = let z = x + y -- let introduces local bindings.
       in print z
~~~

This program will print 5.

Bound names can't start with uppercase letters, and are separated by
semicolons, which are usually automatically inserted as above. Functions
can even be bound, e.g.:

~~~ {.haskell}
add x y = x + y   -- defines function add
five = add 2 3    -- invokes function add
~~~

This is a good way to define functions.

Parentheses are important to eliminate ambiguity in function application.

~~~ {.haskell}
bad = print add 2 3     -- error! (print should have only 1 argument)

main = print (add 2 3)  -- ok, calls print with 1 argument, 5
~~~

Haskell is a pure functional language. Thus, unlike variables in
imperative languages, Haskell bindings are immutable: within a given
scope, each symbol can be bound only once. In other words, the following
is an error:

~~~ {.haskell}
x = 5
x = 6 -- Error, cannot re-bind x
~~~

Bindings are thus order-independent; if any two are switched such that the program still makes sense (i.e. things aren't used after they are bound), the program behaves in the same way.

Another interesting fact is that bindings are lazy: definitions of
symbols are only evaluated when they're needed. For example:

~~~ {.haskell}
safeDiv x y =
    let q = div x y     -- safe as q isn't evaluated if y == 0
    in if y == 0 then 0 else q
main = print (safeDiv 1 0) -- prints 0
~~~

Notice this is completely different from C-like languages!

Another interesting aspect of bindings, which goes hand-in-hand with
order-independence, is that bindings are recursive, so each binding is
in scope within its own definition.

~~~ {.haskell}
x = 5                   -- not used in main!

main = let x = x + 1    -- introduces a new x, defined in terms of itself
       in print x       -- loops forever, or stack overflows
~~~

In C, this would print `6`, but here, `x` refers to itself! The runtime
sometimes is able to detect the loop, however.

This means that writing things in Haskell requires thinking differently!
For example, here's a factorial program in C:

~~~ {.c}
long factorial(int n) {
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}
~~~

But in Haskell, one uses recursion.

~~~ {.haskell}
factorial n = if n > 1 then n * factorial (n-1)
                       else 1
~~~

However, the C function requires constant space, but the Haskell version
requires $n$ stack frames! But Haskell supports optimized tail
recursion; if a function ends with a call to itself (i.e. is
tail-recursive), then it can be optimized into a loop. However, the
definition provided above isn't tail-recursive.

Using an accumulator, the factorial function can be made tail-recursive.

~~~ {.haskell}
factorial n = let loop acc n' = if n' > 1
                                then loop (acc * n') (n' - 1)
                                else acc
              in loop 1 n
~~~

This uses an *accumulator* to keep track of the partial result. It's a
bit clunky, but can be tidied up with Haskell's incredible concision.
For example, one can use guards to shorten function declarations, e.g.

~~~ {.haskell}
factorial n = let loop acc n' | n' > 1 = loop (acc * n') (n' - 1)
                              | otherwise = acc
              in loop 1 n
~~~

The guards (pipe symbols) are evaluated from top to bottom; the first
one that evaluates to `True` is followed. `otherwise` is defined to be
`True`, but it makes the code easier to read. One might also introduce a
`where` clause, which is like `let` but can support multiple guarded
definitions, and is thus convenient for use around guards.

~~~ {.haskell}
factorial n = loop 1 n
    where loop acc n' | n' > 1    = loop (acc * n') (n' - 1)
                      | otherwise = acc
~~~

You'll notice that there will be plenty of inner functions, and their
arguments are related to that of the outer functions. But it's easy to
confuse `n` and `n'`, so the following code compiles and throws a
runtime error!

~~~ {.haskell}
factorial n = loop 1 n
    where loop acc n' | n' > 1    = loop (acc * n) (n' - 1) -- bug, should be n'
                      | otherwise = acc
~~~

One way to work around that is to use a naming convention in which the
outermost variable has the longer name; then, bugs like this are caught
at compile time, due to scope.

Haskell is strongly typed, so we have types such as `Bool`, which is
either `True` or `False`; `Char`, which is a Unicode code point; `Int`,
a fixed-size integer; `Integer`, an arbitrary-sized integer; `Double`,
which is like a `double` in C; and also functions. A function from type
`a` to tybe `b` is denoted `a -> b`. We also have tuples:
`(a1, a2, a3)`, including the unit `()` (a zero tuple, kind of like
`void` of C).

It's good practice to write the function's type on top of a function,
e.g.

~~~ {.haskell}
add :: Integer -> (Integer -> Integer)
add arg1 arg2 = arg1 + arg2
~~~

Well, here's something interesting. The arrow associates to the right,
so these parentheses aren't strictly necessary, but they make an
important point: all functions accept only one argument, so the above
function takes an integer and returns a function! For example, `add 2 3`
is parsed as `(add 2) 3`, and `add 2` is a function. Often, this
behavior (called currying) is optimized out by the compiler, but can be
useful. The compiler can infer types, and in the interpreter this can be
queried by `:t`.

``` {.haskell}
*Main> :t add
add :: Integer -> Integer -> Integer
```

The user can also define data types, using the `data` keyword.

~~~ {.haskell}
data PointT = PointC Double Double deriving Show
~~~

This declares the type `PointT` with a constructor `PointC` containing
two `Double`s. The phrase `deriving Show` means that it can be printed,
which is useful in the interpreter. Types and constructors must start
with capital names, but live in different namespaces, so they can be
given the same name.

Types may have multiple constructors, and said constructors don't
actually need arguments (which makes them look sort of like `enum`s in
C).

~~~ {.haskell}
data Point = Cartesian Double Double
           | Polar Double Double
             deriving Show

data Color = Red | Green | Blue | Violet deriving (Show, Eq, Enum)
~~~

Now, we can do things like `myPoint = Cartesian 1.0 1.0` and so on.

One extracts this data using `case` statements and guards, as in the
following example:

~~~ {.haskell}
getX, getMaxCoord :: Point -> Double
getX point = case point of
               Point x y -> x       -- if only the Point x y constructor is around
getMaxCoord (Point x y) | x > y     = x
                        | otherwise = y

isRed :: Color -> Bool
isRed Red = True        -- Only matches constructor Red
isRed c   = False       -- Lower-case c just a variable
~~~

The latter notion is called pattern matching, which detects which
constructor was used to create the object. For another example, consider
the following:

~~~ {.haskell}
whatKind :: Point -> String -- Cartesian or polar constructor as above
whatKind (Cartesian _ _) = "Cartesian"
whatKind (Polar _ _)     = "Polar"
~~~

This underscore indicates that the value is unused, or something we
don't care about. The compiler can actually infer and optimize based on
that. It's bound, but never used, which is quite helpful, especially
given that the compiler warns about unused variables.

Given the following types for a rock-paper-scissors game:

~~~ {.haskell}
data Move = Rock | Paper | Scissors
     deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)
~~~

Define a function `outcome :: Move -> Move -> Outcome`. The first move
should be your own, the second your opponent's, and then the function
should indicate whether one won, lost, or tied.

*Solution:*

~~~ {.haskell}
outcome :: Move -> Move -> Outcome
outcome Rock Scissors           = Win
outcome Paper Rock              = Win
outcome Scissors Paper          = Win
outcome us them | us == them    = Tie
                | otherwise     = Lose
~~~

There are plenty of other ways to do this.

Types, much like functions, can accept parameters, but type parameters
start with lowercase letters. For example, within the standard Prelude:

~~~ {.haskell}
data Maybe a = Just a
             | Nothing

data Either a b = Left a
                | Right b
~~~

`Maybe` is used to indicate the presence of an item, or some sort of
error, and `Either` can provide more useful error information, etc. In
this case, the convention is for `Right` to indicate the normal value,
and `Left` some sort of sinister error. The interpreter can reason about
these types, too:

~~~ {.haskell}
Prelude> :t Just True
Just True :: Maybe Bool
Prelude> :t Left True
Left True :: Either Bool b
~~~

Often, one uses the underscore pattern matching mentioned above with
these parameterized types to pass exceptions along. For example,

~~~ {.haskell}
addMaybes mx my | Just x <- mx, Just y <- my = Just (x + y)
addMaybes _ _                                = Nothing
~~~

Equivalently (and more simply),

~~~ {.haskell}
addMaybes (Just x) (Just y) = Just (x + y)
addMaybes _ _               = Nothing
~~~

<!--Lists {#lists .unnumbered}
------->

Now, we have enough information to define lists, somewhat like the
following.

~~~ {.haskell}
data List a = Cons a (List a) | Nil

oneTwoThree = (Cons 1 (Cons 2 (Cons 3 Nil))) :: List Integer
~~~

But since lists are so common, there are some shortcuts: instead of
`List Integer`, one writes `[Integer]`, and the `Cons` function is
written `:`, and is infix. The empty list is called `[]`, so instead we
could have written `oneTwoThree = 1:2:3:[]`. Alternatively, there is
nicer syntax:

~~~ {.haskell}
oneTwoThree' = [1, 2, 3]  -- comma-separated elements within brackets
oneTwoThree'' = [1..3]    -- define list by a range
~~~

Strings are just lists of characters.

Here are some useful list functions from the Prelude:

~~~ {.haskell}
head :: [a] -> a -- first element of a list
head (x:_) = x
head []    = error "head: empty list"

tail :: [a] -> [a]             -- all but the first element
tail (_:xs) = xs
tail []     = error "tail: empty list"

a ++ b :: [a] -> [a] -> [a]  -- infix operator to concatenate lists
[] ++ ys     = ys
(x:xs) ++ ys = x : xs ++ ys

length :: [a] -> Int         -- This code is from language specification.
length []    =  0            -- GHC implements differently, because it's not tail-recursive.
length (_:l) =  1 + length l

filter :: (a -> Bool) -> [a] -> [a] -- returns a subset of a list matching a predicate.
filter pred [] = []
filter pred (x:xs)
	| pred x     = x : filter pred xs
	| otherwise  = filter pred xs
~~~

Note the function `error :: String -> a`, which reports assertion
failures. `filter` is a higher-order function, i.e. one of its arguments
is also a function. For example, one might define a function
`isOdd :: Integer -> Bool` and then filter a list of `Integer`s as
`filter isOdd listName`.

In addition to `deriving Show`, one has `deriving Read`, allowing one to
parse a value from a string at runtime. But parsing is tricky: there
could be multiple possible values, or ambiguity. For example, suppose we
have the following declaration:

~~~ {.haskell}
data Point = Point Double Double deriving (Show, Read)
~~~

Then, the following would happen in the interpreter.

~~~ {.haskell}
*Main> reads "invalid Point 1 2" :: [(Point, String)]
[]
*Main> reads "Point 1 2" :: [(Point, String)]
[(Point 1.0 2.0,"")]
*Main> reads "Point 1 2 and some extra stuff" :: [(Point, String)]
[(Point 1.0 2.0," and some extra stuff")]
*Main> reads "(Point 1 2)" :: [(Point, String)] -- note parens OK
[(Point 1.0 2.0,"")]
~~~

Notice that `reads` returns a list of possibilities, along with the rest
of the string. This is asymmetrical from `show`.

This isn't a language property, but the best way to search for functions
(and their source code!) is Hoogle, at <http://www.haskell.org/hoogle/>.
This is a good way to look up functions, their type signatures, and so
on. Looking at the source is a great way to learn the good ways to do
things in Haskell; in particular, notice just how short the functions
are. Haskell has a steep learning curve, but it's pretty easy to
understand what code is doing.

For another example of functional thinking, here's a function to count
the number of lowercase letters in a string.

~~~ {.haskell}
import Data.Char    -- brings function isLower into scope

countLowerCase :: String -> Int -- String is just [Char]
countLowerCase str = length (filter isLower str)
~~~

This looks absurd in C, but since Haskell is lazily evaluated, it
doesn't actually copy the string; in some sense, values and function
pointers are the same under lazy evaulation. But, of course, this can be
written more concisely:

~~~ {.haskell}
countLowerCase :: String -> Int
countLowerCase = length . filter isLower
~~~

Here, `f . g` means <i>f</i> ∘ <i>g</i> mathematically: this is function
composition: `(f . g) x = f (g x)`. But now, the argument isn't
necessary. This can be used like a concise, right-to-left analogue to
Unix pipelines. This is known as point-free programming.

Conversely, if one wants the arguments but not the function, you can
just use lambda expressions. The notion is `\`*variables* `->` *body*.
For example:

~~~ {.haskell}
countLowercaseAndDigits :: String -> Int
countLowercaseAndDigits =
    length . filter (\c -> isLower c || isDigit c)
~~~

This is useful for small functions that don't need to be used in more
than one place.

Another neat syntactic trick is that any function can be made prefix or
infix. For functions that start with a letter, underscore, digit, or
apostrophe, prefix is the default, but they can be made infix with
backticks, e.g. `` 1 `add` 2 ``. For anything else, infix is default,
adding parentheses makes them prefix: `(+) 1 2` and so on. Infix
functions can also be partially applied:

~~~ {.haskell}
stripPunctuation :: String -> String
stripPunctuation = filter (`notElem` "!#$%&*+./<=>?@\\^|-~:")
-- Note above string the SECOND argument to notElem ^
~~~
