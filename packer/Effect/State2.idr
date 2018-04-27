module Effect.State2

import Effects

%access public export

data State2 : Effect where
    Get :      sig State2 a  a
    Put : b -> sig State2 () a b
    
implementation Handler State2 m where
    handle st Get     k = k st st
    handle st (Put n) k = k () n
    
STATE2 : Type -> EFFECT
STATE2 t = MkEff t State2

get : Eff x [STATE2 x]
get = call $ Get

put : x -> Eff () [STATE2 x]
put val = call $ Put val

putM : y -> Eff () [STATE2 x] [STATE2 y]
putM val = call $ Put val

update : (x -> x) -> Eff () [STATE2 x]
update f = put (f !get)

updateM : (x -> y) -> Eff () [STATE2 x] [STATE2 y]
updateM f = putM (f !get)

locally : x -> (Eff t [STATE2 x]) -> Eff t [STATE2 y]
locally newst prog = do st <- get
                        putM newst
                        val <- prog
                        putM st
                        pure val
