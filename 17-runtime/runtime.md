% Implementing Haskell: the Runtime System by Edward Z Yang
% Thomas Dimson (Scriber)
% May 27, 2014

# Implementing Haskell: The Runtime System 

A guest lecture by Edward Z Yang, scribed by Thomas Dimson.

Previously, we have discussed the details of the Haskell compiler
but did not discuss another large component of Haskell: the runtime system.
For example, what makes Haskell green threads cheaper than implementations
in other languages? Although we may not hack the GHC runtime, the internals
are "good for the soul" and helps us understand other systems like the JVM.


# What is a run-time system?

* Runtimes are a blob of code which sits between C client code and
  compiled Haskell code
* The runtime handles things like garbage collection, thread scheduling,
  dynamic linking, software transactional memory, profiling, etc.
* The Haskell run-time also includes a bytecode interpreter if you use
  GHCi

# Garbage collection

* Garbage collection allows us to pretend programs have infinite memory
  and allocate indefinitely
* Instead of explicitly freeing memory, we reclaim dead data
* One approach: reference counting. 
    * We store a count along with each object, representing the number of pointers
      still pointing to it. When the count goes to zero, we free the memory.
    * Unfortunately this can't handled cyclical references and may cost more than
      expected if we deallocate a large structure.
* Upgraded approach: mark and sweep
    * Mark phase: stop the world, go over the heap and mark objects which are alive (i.e. reachable
      from the root set of memory)
    * Sweep phase: clear free memory that wasn't marked. Unfortunately, this causes fragmentation
      and needs to sweep the entire heap. To reallocate, one needs to a traverse a list of free memory
    * Fragmentation can be partially solved by having a compaction phase

## Generational copying collector
* A modern approach utilized by GHC and the JVM 
* Generational hypothesis: "most objects die young". 
    * In other words, if you allocated the memory recently then it is very likely to 
      disappear.
    * Especially true for functional languages because most (or all) data is
      immutable
* Cheney's algorithm
    * Maintain a "from space" and a "to space". The "from space" contains
      objects that are both dead and alive while the "to space" will only
      contain alive references.
    * Begin with "evacuation phase" where we move an object from the "from
      space" to the"to space". We set up a forwarding pointer in the old
      location to the new location
    * We don't naively recurse because this may use a large stack. Instead, we
      use a queue-based approach liked breadth-first search.
    * Next phase is "scavenge" where we start modifying pointers. We maintain a 
      scavenge pointer which advances over objects in the "to space". Everything
      less than the scavenge pointer has been rewritten with new pointers
    * After all is complete, we change the "to space" into the "from space"

* Generational portion
    * Instead of just having one "from" or "to" space, we have different spaces for
      different generations. 
    * Fresh objects are allocated in the nursery. If an object from the nursery
      survives garbage collection, it is promoted to an older generation

* Copying collectors come with advantages
    * The more garbage you have, the faster it runs since it only traverses live
      objects.
    * Free memory is always contiguous

* We can perform GC whenever the free heap pointer advances past the limit of
  the heap (super easy check)

## Write barriers and purity
* If generational garbage collectors are so good, why doesn't every language use
  them?
  * You need to know all the pointers in an object, which can be difficult to
    maintain.
  * We assume that nothing from an older generation points to the nursery. This
    makes sense in a purely functional language, but doesn't hold in the
    presence of mutation (even in Haskell there are IORefs)
  * Solution: we can maintain  "mutable set" which also gets traced during a
    minor GC. This complicates generational GC and forces an extra pointer write
    for mutable memory.
  * Fortunately, in pure languages mutation is rare. IORefs are slow to begin
    with, and lazy mutation (thunks) can be specialized

* Special thunk behavior
    * When a thunk gets evaluated, it is immutable afterwards. Thus, when the
      thunk gets evaluated we can immediately promote the pointed objects into
      an older generation
    * Doesn't work for IORefs: they are expected to be mutated a lot, and so we
      would end up promoted lots of things into the older generation

## Parallel garbage collection
* Key idea: split the heap into blocks and parallelize the scavenge process
* Contrast to _concurrent_ GC, which doesn't stop the world. Parallel garbage
  collection still stops the world
* Problem: two gc threads might try to scavenge something that points to the
    same object, forcing us to take a lock
* Fortunately, if the object pointed to is pure we can just copy the object
  twice and allow thread to race without locks


# Scheduler
* Schedulers are the heart of the run-time system. It is responsible for calling
  into Haskell code, which will eventually yield back to the scheduler
* In Haskell, we can force a yield by setting the heap limit to zero and the
  thread will yield for garbage collection.
* Each thread contains a stack object pointer with all the stack frames from the
  thread. Thus, it can resume where it left off when it is scheduled again
* Threads are fast to allocate because they only consist of a small initial
  stack object (lives on the heap, collected as part of the young generation)
* Scheduler inner loop, single threaded edition:
    * Thread queue maintains all the threads that need to run
    * Scheduler picks up a thread, waits for the thread to run for a while then
      interrupts and puts it back into the queue
* Scheduler inner loop, multi threaded edition:
    * Each scheduler runs in its own OS thread
    * If FFI is marked as safe, then it gives up the scheduler lock while
      running. Unsafe doesn't give up the lock, so probably shouldn't block.
    * Garbage collection always takes locks

## MVars
* MVars are implemented as a struct with a value and a queue of threads blocks
  on it
* These threads are in a "blocked queue" instead of the run queue, so blocked
  threads are never scheduled
* If an MVar is dead and there are still waiters, then we have a provable
  deadlock. The GHC runtime detects this and starts throwing exceptions

## Scheduler take-aways
* Everything live on the heap
* Purity means that most code is threadsafe by default
* Both rust and go have tried to use segmented stacks (i.e. allocated and
  grown) but gave them up because they are too slow. Design decisions have to 
  be made to enable segmented stacks.
