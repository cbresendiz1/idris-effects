module Main

import Effects
import Effect.StdIO
import Effect.State

data GenNum : Effect where
    Get   : sig GenNum Int Int
    Set   : sig GenNum () Int
    
GENNUM : Type -> EFFECT
GENNUM t = MkEff t GenNum

Handler GenNum m where
    handle st Get   k = k st (st + 1)
    handle st Set   k = k () 200

get2 : Eff Int [GENNUM Int]
get2 = call Get

set2 : Eff () [GENNUM Int]
set2 = call Set

runner : Eff (Int, Int) [GENNUM Int]
runner = do x <- get2
            y <- get2
            z <- get2
            set2
            a <- get2
            pure (a, z)

testt : (Int, Int)
testt = runPureInit [20] (do runner)
