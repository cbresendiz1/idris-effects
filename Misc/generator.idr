module Main

-- Local Variables:
-- idris-load-packages: ("contrib" "effects")
-- End:

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

data Generator : Effect where
    Yield : b -> sig Generator () k k

Handler Generator m where
    handle st (Yield n) k = k () st
    
    

