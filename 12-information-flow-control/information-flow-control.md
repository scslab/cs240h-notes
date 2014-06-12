<!---
Scribe Notes for the Information Flow Control lecture
Chris Copeland

A few things copied from the actual lecture notes where I couldn't keep up.
-->

# Safe Haskell

* How can we incorporate untrusted code in our programs?

* We often want to use 3rd party libraries (e.g., from Hackage)

* Want to limit the damage they can do if they are malicious.

## How do we decide whether code is safe?

* We can't determine safety of functions just from type signatures.

* `unsafePerformIO` and others can do nasty things without revealing it in the type of the
function.

* We need some way to trust the type signature (pure functions should actually be pure).

## Enter Safe Haskell

* Prevents your modules from importing "unsafe" modules

* Usage:

~~~ {.haskell}
import safe ...
~~~

* Can invoke GHC with `-XSafe` to force all imports to be safe.

* Forces compilation to fail if the import is not safe.

## Trusting packages

* Problem: useful modules often need to do "unsafe" operations.

* Need a way of asserting the safety of these operations.

* Module authors can compile with `-XTrustworthy`, which simply asserts that the module is safe.

* Does not restrict the module's implementation from doing unsafe things,
(`unsafePerformIO`, `inlinePerformIO`, etc.), but does force it to expose a safe interface.

* Can't anyone just use -XTrustworthy?

* Users of these modules can decide whether to trust them.

* `-fpackage-trust` enables per-package trust

* `-distrust` or `-distrust-all-packages`
ignores the `Trustworthy` property for chosen packages or all packages.

## Restricted I/O

* What if untrusted code needs to do I/O (that cannot be hidden behind a pure interface)?

* Use "restricted" I/O via the `RIO` monad.

* Idea: implement/instantiate `RIO` functions for specific capabilities, and implicitly
disable all other forms of IO by not implementing them for the `RIO` monad.

* Use newtype to wrap `RIO` around `IO`, and only export the type, but not the constructor.

* Export runRIO to allow the IO action to be run, but this can only happen in the RIO monad, and hence it is safe.

* Internally, the `RIO` implementation uses the `UnsafeRIO` constructor.

* This means a user can't take an IO action and turn it into an RIO action, but can take an
RIO action and turn it into an IO action, and therefore can only run RIO actions.

~~~ {.haskell}
runRIO :: RIO a -> IO a
~~~

Example restrict access to particular network ports or files.

This isn't possible in many other languages, where we are always implicitly in an
IO monad.


# (Decentralized) Information Flow Control

## Why RIO isn't enough

* How can we prevent leaking private data on a website while still using 3rd party libraries to
manipulate that data?

* Consider a malicious googleTranslate.

* We can use code that manipulates and stores private data in a sandbox, but the
implementor might have created a backdoor that returns the contents of the sandbox given
a certain input.

## Information Flow Control (IFC)

The basics:
all data has a label
all processes have a label
the labels have a partial order
A "can flow to" B, denoted: A ⊑ B

Examples:
if "file" ⊑ "emacs" then emacs can read
if "emacs" ⊑ "file" then emacs can write
(we often want both directions)

* Access control is transitive (property of a partial order)

* In IFC we have a lattice of labels

Consider two users:

~~~
public : L_0
userA : L_A
userB : L_B
AandB : L_AB
~~~

`L_AB` is the least upper bound of `L_A` and `L_B`

## *Decentralized* Information Flow Control

A DIFC system has a notion of privileges, which form a preorder on the "can flow to" (⊑)
operator.

Using privileges, users can declassify their own data and partially declassify data
higher on the lattice.

under A's privileges, the lattice collapses to (L_0 + L_A) and (L_B + L_AB)
under B's privileges, the lattice collapses to (L_0 + L_B) and (L_A + L_AB)


## Static implementation in Haskell

Encode a two point lattice in the type system:

~~~ {.haskell}
data L = Lpriv
data H = Hpriv
~~~

* Export types but not constructors

* Constructors act like "keys", that can deconstruct the value, which would let users
circumvent the labels.

Use a multi-parameter typeclass to implement ⊑

~~~ {.haskell}
-- multiparam typeclass extension
class Flows sl sh where
instance Flows L L
instance Flows L H
instance Flows H H

newtype Sec s a = MkSec a
instance Monad (Sec s) where
    return x = MkSec x
    MkSec a >>= k = k
~~~

Allow anyone to label a value but require privileges to unlabel

~~~ {.haskell}
label :: a -> Sec s a
label x = MkSec x
unlabel :: Sec s a -> s -> a
unlabel (MkSec a) s = s `seq` a -- need to force the privilege with seq, otherwise it won't be checked
~~~

We can't pull things out of the sec monad without having the constructor for that label.

Allow data to be relabeled if an instance of Flows exists for that ordered pair.
~~~ {.haskell}
relabel :: (Flows lin lout) => Sec lin a -> Sec lout a
relabel (MkSec val) = MkSec val
~~~

## Secure I/O

What if we need both the `Sec` and `IO` monads?

Is this code safe?
~~~ {.haskell}
untrustedTranslate :: Sec H L.ByteString -> Sec H (IO L.ByteString)
~~~
No, as the I/O action wrapped by `Sec` can be arbitrary, and might do nasty things if we
run it.

Solution, combine `Sec` and `RIO` into one big monad, so that we can define what I/O is
allowed in the secure context.

~~~ {.haskell}
value :: Sec s a -> SecIO s a
value s a = MkSecIO (return sa)

plug :: Less sl sh => SecIO sh a -> SecIO sl (Sec sh a)

type File s = SecFilePath String

readFileSecIO :: File s -> SecIO s' (Sec s String)
~~~

New safe external query type signature:
~~~ {.haskell}
queryGoogle :: Sec H L.ByteString -> SecIO H L.ByteString
~~~

All enforcement done at compile time.

## Dynamic DIFC with `LIO`

* We may need to create new labels at runtime and enforce the DIFC properties on them.

* Idea: track both the current label and maximum label or "clearance".

* Associate LIOState with each thread.

~~~ {.haskell}
data LIOState l = LIOSTate { lioLabel, lioClearance :: !l }

newtype LIO l a = LIOTCB (IORef (LIOState l) -> IO a)

instance Monad (LIO l) where
    return = LIOTCB . const . return
    (LIOTCB ma) >>= k = LIOTCB $ \s -> do
        a <- ma
        case k a of LIOTCB mb -> mb s


-- need back door from IO to LIO for privileged code
-- don't export this symbol
ioTCB :: IO a -> LIO l a
ioTCB = LIOTCB . const
~~~

* TCB = Trusted Computing Base


Encoding privileges:

~~~ {.haskell}
class (Typeable p, Show p) => SpeaksFor p where
    speaksFor :: p -> p -> Bool

downgradeP -- compute lowest equivalent label under some privilege
canFlowToP -- determine whether the downgrade of one label can flow to 
~~~

How can we allow computation that encodes the fact that you have seen information
with a particular label?

~~~ {.haskell}
taint :: Label l => l -> LIO l ()
~~~

## Privileges vs. privilege descriptions

* We want to be able to name privileges in any context.

* There is an important difference between naming privileges and exercising them.

~~~ {.haskell}
newtype Priv a = PrivTCB a deriving (Show, Eq, Typeable)

instance Monoid p => Monoid (Priv p) where
  mempty = PrivTCB mempty
  mappend (PrivTCB m1) (PrivTCB m2) = PrivTCB $ m1 `mappend` m2

privDesc :: Priv a -> a
privDesc (PrivTCB a) = a

privInit :: p -> IO (Priv p)
privInit p = return $ PrivTCB p
~~~

* Can delegate privileges to someone else.

* Can wrap privileges in closures.

* Gates, which take privileges and return privilege descriptions so someone can require
you to have privileges to do something, but does not force you to delegate those privileges
in order to prove that you have them.

Augmenting normal I/O actions with labels.

~~~ {.haskell}
{-# LANGUAGE Trustworthy #-}

import LIO.TCB.LObj

type LMVar l a = LObj l (MVar a)

takeLMVar :: Label l => LMVar l a -> LIO l a
takeLMVar = blessTCB "takeLMVar" takeMVar

tryTakeLMVar :: Label l => LMVar l a -> LIO l (Maybe a)
tryTakeLMVar = blessTCB "tryTakeLMVar" tryTakeMVar

putLMVar :: Label l => LMVar l a -> a -> LIO l ()
putLMVar = blessTCB "putLMVar" putMVar
~~~

## Applications using `LIO`

* Main application is Hails web framework
	* Really a framework for creating web platforms hosting mutually distrustful apps
* Example: GitStar (which is implemented with Hails)
	* Host potentially private git repositories
	* Functionality for third party extensions (e.g., syntax-highlighting) cannot leak private source code
* Ongoing research here at Stanford

