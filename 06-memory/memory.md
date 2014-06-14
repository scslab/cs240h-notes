% Memory
% Brandon Azad

Software Transactional Memory
=============================

Suppose we have an account type

~~~~ {.haskell}
type Account = MVar Double
~~~~

and we want to write a function

~~~~ {.haskell}
transfer :: Double -> Account -> Account -> IO ()
~~~~

to transfer money between the accounts safely. One solution would be to write
a function with nested calls to `modifyMVar_`. However, there are multiple
issues. One is that if you transfer money between the accounts in opposite
directions, you can get deadlock, because the `MVar`s are acquired in opposite
order. To fix this you could have some sort of ordering on accounts (for
example using `Data.Unique`). This is somewhat difficult since there’s no
default `Ord` instance on `MVar`s. Another solution could be to use the `try*`
family of functions, and then swap the order of acquisition if the transfer
fails. This gets ugly fast.  Another solution would be to lock all account
operations, but this effectively removes all benefits of parallelism.

A better approach is software transactional memory, where you read/write all
the variables you need to and try to commit your changes at the end, and if
someone else touched any of the variables you modified the whole operation is
aborted. The Haskell language has an advantage here over imperative languages
in that pure code can easily be repeated without risk of doing something bad.

The new type `TVar` is the “improved” STM version of `MVar`. The STM monad
allows `TVar` operation but forbids irreversible side effects. To convert some
computation in the `STM` monad to the `IO` monad use `atomically`. In the
`STM` monad you can’t perform `IO` operations, but that’s fine because `STM`
operations can be constructed and combined and then executed atomically within
the `IO` monad.

The atomic transfer function would then be

~~~~ {.haskell}
type Account = TVar Double
transfer :: Double -> Account -> Account -> STM ()
transfer account from to = do
  modifyTVar' from (subtract amount) -- not (- a) because (-) is unary negation
  modifyTVar' to (+ amount)
~~~~

Note that `STM` transactions can’t be nested in the sense of running
`atomically` inside another call to `atomically`. This makes sense since `STM`
monad operations glue together in the natural way, so that running a composite
computation under `atomically` executes the entire transaction atomically, not
just the individual parts.

Also, what if we want to go to sleep if there’s not enough money in the
account? There’s a function `retry` that will abort and retry the transaction.
The `STM` monad knows what you’ve looked at and will sleep until one of the
`TVar`s you’ve looked at during the failed transaction changes. This is much
more intelligent behavior than is easy to build out of `MVar`s.

~~~~ {.haskell}
type Account = TVar Double
transfer :: Double -> Account -> Account -> STM ()
transfer account from to = do
  bf <- readTVar from
  when (amount > bf) retry
  modifyTVar' from (subtract amount)
  modifyTVar' to (+ amount)
~~~~

The `orElse` function allows to define alternate actions when a transaction
fails.

We can also enforce invariants on transactions using the `alwaysSucceeds`
function. The `alwaysSucceeds` transaction is always run at the end of every
transaction. If we state some invariant and any transaction fails the
invariant at any later point, then we can raise an exception by calling
`fail`.

Representing Haskell Data
=========================

How would we implement a Haskell data type in C? A value requires a
constructor and then some number of arguments. Say that we represent a value
as follows:

~~~~ {.c}
struct Val {
  unsigned long constrno;
  struct Val * args[];
};
~~~~

This would work fine for most types (e.g. `Int`, `[Int]`), but there is no way
to represent thunks or exceptions. Also, the garbage collector would have to
know how many arguments there are. There’s also the efficiency issue: To get
the value out of an `Int`, we need to chase down a pointer.

Adding Indirection
------------------

We can patch this up somewhat by adding some indirection.

~~~~ {.c}
struct Val {
  const struct ValInfo * info;
  struct Val * args[];
};

struct ValInfo {
  struct GCInfo gcInfo;
  enum { CONSTRNO, FUNC, THUNK, IND } tag;
  union {
    unsigned constrno;
    Val *(*func) (const Val * closure, const Val * arg);
    Exception *(*thunk) (Val * closure);
  };
};
~~~~

We don’t need to know the type of any of these values at runtime since the
typechecker has already run. If tag is `IND`, then `args[0]` is an indirect
forwarding pointer to another `Val`, and the union is not used. This is a bit
of a hack for reasons to be discussed later. Functions and thunks have
pointers to code that represent the as-yet uncomputed value.

Evaluating Functions
--------------------

To apply the function `f` to argument a we use `f->info->func(f, a)`. We want
to pass the function itself because sometimes there’s useful information in
the closure itself. For example, if we have

~~~~ {.haskell}
add :: Int -> Int -> Int
add n = (\m -> addn m)
  where addn m = n + m
~~~~

then we’d want to be able to extract the first argument `n` from `addn` when
evaluating `add`. (The same thing happens with the natural definition of
`add`, of course.)

Evaluating Thunks
-----------------

To evaluate a thunk, we use `v->info->thunk(v)`, which will update the thunk
in-place. There are three main differences between the implementation of
thunks and functions.

1.  A function takes an argument, while a thunk does not.

2.  A function value is immutable, while a thunk updates itself.

3.  A thunk might return an exception. (To model functions that return
    exceptions, we have the function return a thunk that returns the
    execption.)

If the code is running on a multiprocessor, then there should be locking in
place to prevent simultaneous evaluation of thunks. This can be represented as
a fifth type, which is a locked thunk.

Forcing is the process of turning a thunk into a non-thunk. When you do this,
if the thunk’s return value doesn’t fit into the thunk’s args, then we use the
`IND`.

Currying
--------

Let’s look at a simple implementation of currying. A function taking $n$
values would be created as $n$ different functions, `func_1`, `func_2`, up to
`func_n`.
If we have a function

~~~~ {.haskell}
const3 a b c = a
~~~~

then the compiler would generate three functions each taking one argument but
referencing other functions.
The last one in the chain would have access to all the arguments and actually
carry out the computation.

A nice benefit of this approach is that if you have arguments with common
argument tails, then the arguments are only evaluated once.

Unboxed Types
-------------

This implementation is even worse now for small data types, since we have lots
of overhead. For example, if we have an `Int` type, we don’t want to go
chasing down lots of pointers. GHC fixes this by supporting unboxed types.

~~~~ {.c}
union Arg {
  struct Val * boxed;
  unsigned long unboxed;
};

typedef struct Val {
  const struct ValInfo * info;
  union Arg args[];
}
~~~~

GHC exposes these unboxed types using the `MagicHash` extension.  Unboxed
primitive types and operations on them end in `#`, for example `Int#` and
`+#`. We also have unboxed constants, for example `2#`, `’a’#`. For unsigned
integers and doubles, we have `2##` and `2.0##`.

In GHCi, we can find the implementation of `Int`:

~~~~ {.haskell}
data Int = I# Int#
~~~~

One issue with these unboxed types is that we cannot instantiate type
variables with them:

~~~~ {.haskell}
data UnboxedIntType = UnboxedInt Int#

unboxed0 :: UnboxedIntType
unboxed0 = UnboxedInt 3# -- OK

unboxed1 :: UnboxedIntType
unboxed1 = UnboxedInt $ id 3# -- Error: id is of type a -> a, cannot
                              -- instantiate a with Int#: cannot match *
                              -- against #.
~~~~

This restriction is enforced by having unboxed types be of a different kind
than the familiar types. For example, `Int` is of kind `*` while `Int#` is of
kind `#`, and `I#` is of type `Int# -> Int`.

Strictness Revisited
--------------------

The strictness flag `!` forces the corresponding field to be strict. In terms
of implementation, this forces the `ValInfo` to not have tag `THUNK` or `IND`.
Accessing a strict `Int` touches only one cache line.

Strictness is usually an optimization, but it can also have semantic effects.
An `Int` is not just a number: any `Int` is in the set $\{0,1\}^{64} \cup
\{\bot\}$. Types that include $\bot$ are called lifted. An unboxed type is
necessarily unlifted. Additionally, `!Int` is not a first-class type, it is
only valid for data fields.

`case` Statements
-----------------

Pattern matching in a `case` statement can force a thunk. An irrefutable
pattern, for example a single variable or `_`, always matches.
Non-irrefutable patterns force evaluation of the argument.

Note that function pattern matching is desugared into `case` statements.

We can make a pattern irrefutable by adding a tilde (`~`).

`newtype` Declarations
----------------------

`newtype` introduces a new type with no overhead, since the runtime
representation is the same as the wrapped type. This can be useful to
introduce new context around an existing type.

