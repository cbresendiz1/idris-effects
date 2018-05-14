module Main

import Effects
import Effect.StdIO
import Effect.State



--data State2 : Effect where
--    Get :      sig State2  a a
--    Put : b -> sig State2 () a b

data State2 : Effect where
    Get : (State2 a)     a (\x => a)
    Put : b -> State2 () a (\x => b)
    
STATE2 : Type -> EFFECT
STATE2 t = MkEff t State2

Handler State2 m where
    handle st Get     k = k st st
    handle st (Put n) k = k () n
    
getS : Eff x [STATE2 x]
getS = call Get

putS : x -> Eff () [STATE2 x]
putS val = call (Put val)

putMS : y -> Eff () [STATE2 x] [STATE2 y]
putMS val = call (Put val)

-- ----------------------
-- --- Testing effect ---
-- ----------------------

data Storage : Effect where
    -- Store ( takes an argument b and returns element of unit )
    -- Store It has resource of type a on input
    Store : Nat -> Storage () (List Nat) (\x => (List Nat))
    -- Store b -> sig Gen () a b

STORE : Type -> EFFECT
STORE t = MkEff t Storage

Handler Storage m where
    handle res (Store n) k = k () (n :: res)

store : Nat -> Eff () [STORE (List Nat)]
store n = call (Store n)

-- ---------------------
-- --- Step 2 Effect ---
-- ---------------------

data Runner : Effect where
    GetR : Runner Nat (List Nat) (\x => (List Nat))
    
RUNNER : Type -> EFFECT
RUNNER t = MkEff t Runner

Handler Runner m where
    handle res GetR k = retDef res where
        retDef [] = k 3 res
        retDef (x :: xs) = k x res

getR : Eff Nat [RUNNER (List Nat)]
getR = call GetR

--- --------------------
--- --- Continuation ---
--- --------------------

--data Count : Effect where
--    Grab  : Count () ((), c) (\x => ((), c))
--    Yiel : Nat -> Count Nat ((), c) (\x => ((), c))
    
--Handler Count m where     -- you need to take the continuation
--    handle res (Grab) k = k () res

--Handler Count m where
--    handle res (Grab) k = k () res
    
    
--runStuff : Eff Nat [COUNT Nat]

--schedule funct = runPureInit [funct] runStuff    
    -- handle st (Grab) k = k st st
    -- k - does a computation.... this needs to be continued
    -- st - you can get some computation
    -- st - need to stay
    -- k : ((x : t) -> resource' x -> m a)

--- -----------------------
--- --- Concurrency -------
--- -----------------------

data Conc : Effect where
     Yieldr : Conc () (List a) (\x => (List a))

CONC   : Type -> EFFECT
CONC t = MkEff t Conc

Handler Conc m where
    handle res (Yieldr) k = k () res --do put (k :: !get)
                            --   case !get of
                            --     [] = k () []
                            --     (x :: xs) = x () xs

yieldr : Eff () [CONC (List a)]
yieldr = call Yieldr

data Concert : Effect where
    Nop : Concert () (Maybe a) (\x => (Maybe a))

CONCERT : Type -> EFFECT
CONCERT t = MkEff t Concert

--Handler Concert Maybe where
--    handle res (Nop) k = k () Nothing
   
data TR a = Leaf | Node (TR a) a (TR a)

iter : (a -> b) -> TR a -> ()
iter f tr with (tr)
    | Leaf = ()
    | Node l x r = do iter f l
                      f x
                      iter f r



-- What is a computation context and why doesn't it work with State Int, but it works with Maybe

--- ---------------------
--- --- New Scheduler ---
--- ---------------------
foo : Integer -> Integer -> Eff Integer [CONC (List a), STDIO]
foo id 0 = do Effect.StdIO.putStrLn ("Yielding " ++ show id)
              yieldr
              Effect.StdIO.putStrLn ("Resumed number " ++ show id)
              pure 2

--runner : Integer
--runner = runPureInit ['Tag := []] (foo 1 2)

sched : (a -> b) -> Eff () [STATE (List a), STDIO]
sched f = fork f 
  where
    fork : (a -> b) -> Eff () [STATE (List a), STDIO]
    fork f = do pure $ ?he

main : Eff () [STDIO]
main = putStrLn "Record"

--- -----------------------
--- --- Non-determinism ---
--- -----------------------

data Selection : Effect where
    Select : List a -> sig Selection a
    
Handler Selection Maybe where
    handle _ (Select xs) k = tryAll xs where
        tryAll [] = Nothing
        tryAll (x :: xs) = case k x () of
                           Nothing => tryAll xs
                           Just v  => Just v
                           
Handler Selection List where
    handle r (Select xs) k = concatMap (\x => k x r) xs
    
SELECT : EFFECT
SELECT = MkEff () Selection

select : List a -> Eff a [SELECT]
select xs = call (Select xs)

nonD : Eff Int [SELECT]
nonD = select [1..100]

runtern : Maybe Int
runtern = run {m = Maybe} nonD

-- ---------------------
-- ---- New Effect -----
-- ---------------------

data Gen : Effect where
    Yield  : b -> Gen b c (\x => c)
    -- next

GENNER : Type -> EFFECT
GENNER t = MkEff t Gen



--handle : resource -> (eff : e t resource resource') -> ((x : t) -> resource' x -> m a) -> m a
Handler Gen m where
    handle fres (Yield n) k = f k n
      where
        f l m = l m fres
--    handle fres Next      k = k () fres


yieldE : a -> Eff a [GENNER b]
yieldE n = call (Yield n)
    
--randomFunction : Eff () [STUFF]
--randomFunction = do yield 3
--                    randomFunction

--test : Eff () [GENNER (List a)]
--test = do yield 3
--          test    
            
--tryAgain : Int
--tryAgain = runPureInit [someFun] running

--running : Eff a [GENNER (a -> a)]
--running = next


--test : () [GENNER x]
--test = do yield 3
--          test

-- yield has to be in a function
-- example in python
-- def createGenerator():
--     mylist = range(3)
--     for i in mylist:
--         yield i * i

-- mygenerator = createGenerator()
-- print(mygenerator) # returns an object


-- for i in mygenerator:
--     print(i)



--    next  : sig Cont () a
    
--GENER : Type -> EFFECT
--GENER t = MkEff t Cont

--Handler Cont m where
--    handle st Yield  k = k st (st + 1)
--    handle st Next   k = k () 200

-- You will need to setup a 
