module Main

import Effects
import Effect.State

-- Generator a number for a next call
-- Requires a number

data Generator : Effect where
  Next : Int -> Generator Int Int (\v => Int)

-- While loop for incrementing the number

while : Int -> Int
while n with (n `mod` 2)
  | 0 = n
  | _ = while (n + 1)

-- Handler interface for evens generator

Handler Generator m where
  handle num (Next n) k = k n num

-- st contains the mutations


-- GENERATOR type
GENERATOR : Type -> EFFECT
GENERATOR t = MkEff t Generator

--match : (() -> Bool) -> () -> Bool
--match f () = case f () of
--                True => False
--                _   => True

-- next function : computes evens, then passes values

prgmr : Eff Int [STATE Int]
prgmr = do n <- get
           let w = while (n + 1)
           pure w

next : Eff Int [GENERATOR Int, STATE Int]
next = do w <- prgmr
          call (Next w)

prgm : Eff Int [GENERATOR Int, STATE Int]
prgm = do count <- next
          pure count

startr : Int
startr = runPureInit [0, 0] prgm

main : IO ()
main = putStrLn $ show startr

