module Main

import Effects
import Effect.State
import Effect.StdIO
import Control.IOExcept

--evens : Eff Int [STATE Int]
--evens = do x <- get
--           put (x + 2)
--           pure x

fib : Stream Nat
fib = 1 :: 1 :: zipWith (+) fib (tail fib)

fibL : Lazy (Stream Nat)
fibL = 1 :: 1 :: zipWith (+) fib (tail fib)

streamOfNat : Lazy (Stream Nat)
streamOfNat = 0 :: streamOfNat

evens : Eff (Stream Nat) [STATE Nat]
evens = do x <- get
           put (x + 2)
           (0 :: evens)
