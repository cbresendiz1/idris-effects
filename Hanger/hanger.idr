module Main

import Effects
import Effect.StdIO
import Effect.Random
import Data.So
import Data.Fin
import Data.Vect
import VectMissing

data GState = Running Nat Nat | NotRunning

data Mystery : GState -> Type

data MysteryRules : Effect where
    Guess : (x : Char) ->
            sig MysteryRules Bool
            (Mystery (Running (S g) (S w)))
            (\inword => if inword
                        then Mystery (Running (S g) w)
                        else Mystery (Running g (S w)))
    Won : sig MysteryRules () (Mystery (Running g 0))
                              (Mystery NotRunning)
    Lose : sig MysteryRules () (Mystery (Running 0 g))
                               (Mystery NotRunning)
    NewWord : (w : String) ->
              sig MysteryRules () (Mystery NotRunning) (Mystery (Running 6 (length (letters w))))
    Get : sig MysteryRules String (Mystery h)

data Mystery : GState -> Type where
    Init     : Mystery NotRunning
    GameWon  : (word : String) -> Mystery NotRunning
    GameLost : (word : String) -> Mystery NotRunning
    MkG      : (word : String) ->
               (guesses : Nat) ->
               (got : List Char) ->
               (missing : Vect m Char) ->
               Mystery (Running guesses m)

MYSTERY : GState -> EFFECT
MYSTERY h = MkEff (Mystery h) MysteryRules

letters : String -> List Char
initState : (x : String) -> Mystery (Running 6 (length (letters x)))

data IsElem : a -> Vect n a -> Type where
    First : IsElem x (x :: xs)
    Later : IsElem x xs -> IsElem x (y :: xs)
    
isElem : DecEq a => (x : a) -> (xs : Vect n a) -> Maybe (IsElem x xs)

shrink : (xs : Vect (S n) a) -> IsElem x xs -> Vect n a

Handler MysteryRules m where
    handle (MkG w g got []) Won  k = k () (GameWon w)
    handle (MkG w Z got m)  Lost k = k () (GameLost w)

    handle st Get k = k (show st) st
    handle st (NewWord w) k = k () (initState w)
    
    handle (MkG w (S g) got m) (Guess x) k =
        case isElem x m of
            Nothing => k False (MkG w _ got m)
            (Just p) => k True (MkG w _ (x :: got) (shrink m p))


game : Eff () [MYSTERY (Running (S g) w), STDIO]
              [MYSTERY NotRunning, STDIO]
              
runGame : Eff () [MYSTERY NotRunning, RND, SYSTEM, STDIO]
runGame = do srand !time
             let w = index !(rndFin _) words
             call $ NewWord w
             game
             putStrLn !(call Get)
             
words : ?wtype
words = with Vect ["idris", "agda", "haskell", "miranda",
                   "java", "javascript", "fortran", "basic",
                   "coffeescript", "rust"]
                   
wtype = proof search

game : Eff () [MYSTERY (Running (S g) w), STDIO]
              [MYSTERY NotRunning, STDIO]
game {w=Z} = Won
game {w=S _}
    = do putStrLn !Get
         putStr "Enter guess: "
         let guess = trim !getStr
         case choose (not (guess == "")) of
             (Left p) => processGuess (strHead' guess p)
             (Right p) => do putStrLn "Invalid input!"
                             game
   where
       processGuess : Char -> Eff () [MYSTERY (Running (S g) (S w)), STDIO]
                                     [MYSTERY NotRunning, STDIO]
       processGuess {g} {w} c
           = case !(Main.Guess c) of
               True => do putStrLn "Good guess!"
                          case w of
                              Z => Won
                              (S k) => game
               False => do putStrLn "No, sorry"
                           case g of
                               Z => Lost
                               (S k) => game
                              
