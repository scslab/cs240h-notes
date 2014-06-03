% Language extensions
% Susan Tu

# Monad transformers 

Monad transformers are type constructors that build monads parameterized by other monads.

* Method `lift` executes actions from underlying transformed monad:

~~~ {.haskell}
class MonadTrans t where
    lift :: Monad m => m a -> t m a
~~~

* State transformer monad, `StateT`

~~~ {.haskell}
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    m >>= k  = StateT $ \s0 -> do          -- in monad m
                 ~(a, s1) <- runStateT m s0
                 runStateT (k a) s1

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> do            -- in monad m
                a <- ma
                return (a, s)

get :: (Monad m) = > StateT s m s
put :: (Monad m) = > s -> StateT s m ()
~~~

* Haskell version of doing `x++` in C

~~~ {.haskell}
import Control.Monad.Trans
import Control.Monad.Trans.State

main :: IO ()
main = runStateT go 0 >>= print
  where go = do xplusplus >>= lift . print
                xplusplus >>= lift . print
        xplusplus = do n <- get; put (n + 1); return n

*Main> main
0
1
((),2)
~~~ 

# MonadIO
* Sometimes want to execute IO regardless of current monad (does however many lifts are necessary)

~~~ {.haskell}
class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id
~~~ 
* Let's make liftIO work for StateT

~~~ {.haskell}
instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO

myprint :: (Show a, MonadIO m) => a -> m ()
myprint a = liftIO $ print $ show a
~~~

* All standard Monad transformers implement class MonadIO: ContT, ErrorT, ListT, RWST, ReaderT, StateT, WriterT, ...


* Recusive bindings allowed in Haskell, i.e., things on left are in scope on the right

* Top-level, let, and where bindings are all recursive in Haskell, e.g.:

~~~ {.haskell}
oneTwo :: (Int, Int)
oneTwo = (fst y, snd x)
    where x = (1, snd y)    -- mutual recursion
          y = (fst x, 2)

nthFib :: Int -> Integer
nthFib n = fibList !! n
    where fibList = 1 : 1 : zipWith (+) fibList (tail fibList)
~~~

* Recursion can be implemented using a fixed-point combinator

~~~ {.haskell}
-- in standard library
fix :: (a -> a) -> a
fix f = let x = f x in x
-- now, we use it. We define (x, y) to be the fixed point of a carefully constructed function.
oneTwo ' :: (Int, Int)
oneTwo ' = (fst y, snd x)
  where (x, y) = fix $ \ ~(x0, y0) -> let x1 = (1, snd y0)
                                          y1 = (fst x0, 2)
                                          in (x1, y1)

nthFib ' n = fibList !! n
where fibList = fix $ \l -> 1 : 1 : zipWith (+) l (tail l)
~~~

* The `~` above is a way of doing an irrefutable pattern match (here, don't force the thunk for the pair).

* Monadic bindings are not recursive, but there is `mfix`

# The `RecursiveDo` extension

* New rec keyword introduces recursive bindings in a do block [Erkök02]
* Monad must be an instance of MonadFix (RecursiveDo desugars to mfix calls)

~~~ {.haskell}
oneTwo'' :: (MonadFix m) => m (Int, Int)
oneTwo'' = do
  rec x <- return (1, snd y)
      y <- return (fst x, 2)
  return (fst y, snd x)
~~~

* Desugars to:

~~~ {.haskell}
oneTwo''' :: (MonadFix m) => m (Int, Int)
oneTwo''' = do
  (x, y) <- mfix $ \ ~(x0, y0) -> do x1 <- return (1, snd y0)
                                     y1 <- return (fst x0, 2)
                                     return (x1, y1)
  return (fst y, snd x)
~~~

* In practice RecursiveDo helps structure thinking
* Then can manually desugar rather than require a language extension
* But mfix on its own is quite useful 

# Example uses of mfix and rec

* Create recursive data structures in one shot

~~~ {.haskell}
data Link a = Link !a !(MVar (Link a)) -- note ! is okay

mkCycle :: IO (MVar (Link Int))
mkCycle = do
  rec l1 <- newMVar $ Link 1 l2        -- but $! would diverge
      l2 <- newMVar $ Link 2 l1
  return l1
~~~

* Call non-strict methods of classes (easy access to return-type dictionary)

~~~ {.haskell}
class MyClass t where
    myTypeName :: t -> String        -- non-strict in argument
    myDefaultValue :: t
instance MyClass Int where
    myTypeName _ = "Int"
    myDefaultValue = 0

getVal :: (MyClass t) => IO t
getVal = mfix $ \t -> do      -- doesn't use mfix's full power
  putStrLn $ "Caller wants type " ++ myTypeName t
  return myDefaultValue
~~~

* Implementing mfix

* Warm-up: The Identity monad

~~~ {.haskell}
newtype Identity a = Identity { runIdentity :: a }
instance Monad Identity where
    return = Identity
    m >>= k = k (runIdentity m)
--newtype compiles to nothing, so basically same as fix:
instance MonadFix Identity where
    mfix f = let x = f (runIdentity x) in x
~~~

# `fixIO`, `IO` Monad fixed point 

* Internally, lazy IO is implemented by magic `unsafeInterleaveIO`

# A generic mfix is not possible
* So mfix needs to take fixed point over value, not over monadic action
* How to do this is monad-specific
* Doesn't work for all monads (ContT, ListT)

* Remark: let x = f x in x is better than fix f = f (fix f) because the one on the right might lead to stack overflow (the one on the right allocated thunks on the heap)

# `MultiParamTypeClasses` extension
* Extension itself is relatively safe, but encourages other extensions 
* each method's type must use every type parameter, all types must be fully deteremined, and the usual instance restrictions still apply 

# FlexibleInstances extension
* Allows more specific type paremeters (relatively safe extension)

# `Overlapping Instances` extension 
## This extension is used but also widely frowned upon
* Only need this extension if overlapping instances actually used 
* Enable extension where instances defined not where used 
* Compiler picks the most specific matching insrtance. I_1 is more specific than I_2 when I_1 can be created by substituting for variables of I_2 and not vice versa 
* Contexts (part before =>) not considered when selecting instances 

# A case against `OverlappingInstances`

~~~ {.haskell}
module Help where
    class MyShow a where
      myshow :: a -> String
    instance MyShow a => MyShow [a] where
      myshow xs = concatMap myshow xs

    showHelp :: MyShow a => [a] -> String
    showHelp xs = myshow xs     -- doesn't see overlapping instance

module Main where
    import Help

    data T = MkT
    instance MyShow T where
      myshow x = "Used generic instance"
    instance MyShow [T] where
      myshow xs = "Used more specific instance"

    main = do { print (myshow [MkT]); print (showHelp [MkT]) }
*Main> main
"Used more specific instance"
"Used generic instance"
~~~

#Flexible contexts extension
* MultiParamTypeClasses leads to inexpressible types

~~~ {.haskell}
toInt val = convert val :: Int
~~~

* What is the type of function toInt? Would like to write:

~~~ {.haskell}
toInt :: (Convert a Int) => a -> Int
~~~

* But `(Convert a Int) =>` is an illegal context, as `Int` not a type variable
* FlexibleContexts extension makes the above type legal to write
* Is a relatively safe extension to use
* Still a couple of restrictions

# FunctionalDependencies extension
* Some restrictions: Sufficient conditions of decidable instances 


# Undecidable vs. exponential -- who cares?
* Editorial: maybe decidability of languages is overrated
* Computers aren't Turing machines with infinite tapes, after all

# UndecidableInstances extension
* Lifts Paterson and coverage restrictions
* currently must define an instance for every transformer
* With UndecidableInstances, one instance can cover all transformers 

# Examples 
## Type-level booleans
* The utility of `TypeEq`: can be used to say that two types have to be the same type   
* Editorial: TypeEq is kind of the holy grail of fundeps
* If you can implement TypeEq, you can program recursively at type level by distinguishing base and recursive cases!
* But relies deeply on OverlappingInstances...

