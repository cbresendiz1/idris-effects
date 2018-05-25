module Main

import Effects
import Effect.State
import Effect.StdIO
import Effect.Exception

--- Data Type ---
data Selection : Effect where
     Select : List a -> sig Selection a
     
--- Handlers ---
     
Handler Selection Maybe where
    handle _ (Select xs) k = tryAll xs where
        tryAll []        = Nothing
        tryAll (x :: xs) = case k x () of
                                Nothing => tryAll xs
                                Just v  => Just v
                                
Handler Selection List where
    handle r (Select xs) k = concatMap (\x => k x r) xs
    
--- EFFECTS ---
SELECT : EFFECT
SELECT = MkEff () Selection

select : List a -> Eff a [SELECT]
select [] = call (Select [])
select xs = call (Select xs)


-- Local Variables:
-- idris-load-packages: ("effects")
-- End:

runner : Eq a => List a -> Eff (a, a, a) [SELECT, EXCEPTION String]
runner xs = do x <- select xs
               z <- select xs
               y <- select xs
               if (x /= y && z == y)
                 then pure (x, y, z)
                 else raise "Hmmm.."

               
triple : Int -> Eff (Int, Int, Int) [SELECT, EXCEPTION String]
triple max = do z <- select [1..max]
                y <- select [1..z]
                x <- select [1..y]
                if (x * x + y * y == z * z)
                   then pure (x, y, z)
                   else raise "No triple"

--prn : Eff () [STDIO]
--prn = putStrLn $ show $ the (Maybe _) $ run (runner [1,2,3])

prn2 : Eff () [STDIO]
prn2 = putStrLn $ show $ the (List _) $ run (runner [1,2,3])

prn : Eff () [STDIO]
prn = putStrLn $ show $ the (Maybe _) $ run (runner [1,2,3])

trip : Eff () [STDIO]
trip = putStrLn $ show $ the (Maybe _) $ run (triple 30)

trip2 : Eff () [STDIO]
trip2 = putStrLn $ show $ the (List _) $ run (triple 13)

main2 : IO ()
main2 = run prn

main : IO ()
main = run  prn

using (ma : Type -> Type, res : Type, resk : res -> Type, ty : Type, un : ())
    data Gentle : Type -> Type where
        J : (d : lw -> Gentle lw -> ma a) -> Gentle lw
        L : (d : () -> Gentle ja -> ma a) -> Gentle ja
--        J : (a -> Gentle a -> Maybe a) -> Gentle a
--        L : (() -> Gentle a -> Maybe a) -> Gentle a
        
    data Generator : Effect where
        Next  : Generator ty       (Gentle ty) (\x  => Gentle ty)
        Yield : ty -> Generator () (Gentle ty) (\x  => Gentle ty)

Handler Generator ma where
    handle (J j) (Yield v) k = j v (L k)

--    Handler Generator Maybe where
--       handle (J j) (Yield n) k = j n  (J k)
--       handle (L l) (Next)    k = l () (J k)       
       --    handle res (Next)    k = res 


















--  handle _ (Yield n) k = runInit [k] (contin n) where
--  handle _ (Yield n) k = do x <- (runInit [k] (contin n));
--                            return x
--  handle st Next k      = k st st
  
-------------------------------
---  Exception and More Ex  ---
-------------------------------

--data Exception : Type -> Effect where
--     Raise : a -> sig (Exception a) b

--Handler (Exception a) Maybe where
--    handle _ (Raise e) k = Nothing
    
--Handler (Exception a) List where
--    handle _ (Raise e) k = []
    
--EXCEPTION : Type -> EFFECT
--EXCEPTION t = MkEff () (Exception t)

--raise : List a -> Eff (List a) [EXCEPTION (List a)]
--raise [] = call (Raise [])
--raise (x :: xs) = raise xs

--exampleRun1 : Maybe (List Integer)
--exampleRun1 = the (Maybe _) $ run (raise [1,2,3])

--exampleRun2 : List a -> List a
--exampleRun2 xs = the (List _) $ run (raise xs)

runT : Int -> Eff Int [EXCEPTION Int]
runT 0 = ?rhs

--Handler Generator m where
--    handle cps (Yield n) k = k () [k]
--    handle cps (Yield n) k = cps () k
--    handle (x, k2) (Next)    k = do k x  (x, k2)
-- Example : 
--Handler Generator m where
--    handle cps (Yield n) k = (id k) () cps 

--GENERATOR : (x : Type) -> EFFECT
--GENERATOR t = MkEff t Generator

--yield : Int -> Eff () [GENERATOR k]
--yield n = call (Yield n)

--hi_level : Eff Int [GENERATOR k]
--hi_level = do x <- next
--              pure x

--gen : Eff Int [GENERATOR k]
--gen = do _ <- yield (3 * 3)
--         gen

----------------------------------
--------- Selection --------------
----------------------------------

-- Local Variables:
-- idris-load-packages: ("effects")
-- End:
