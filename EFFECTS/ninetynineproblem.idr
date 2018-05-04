module Main

import Effects
import Effect.StdIO
import Effect.State

problem1 : Eff (Maybe a) [STATE (List a)]
problem1 = do pure $ loop !get
    where
      loop : List a -> Maybe a
      loop xs with (xs)
          | [] = Nothing
          | [x] = Just x
          | (x :: ys) = loop ys

problem2 : Eff (Maybe (a, a)) [STATE (List a)]
problem2 = do pure $ loop !get
    where
      loop : List a -> Maybe (a, a)
      loop xs with (xs)
          | [] | [x] = Nothing
          | [x,y] = Just (x, y)
          | (y :: ys) = loop ys

problem2_1 : Eff (Maybe (a, a)) ['Tag ::: STATE (List a)]
problem2_1 = do xs <- 'Tag :- get
                pure $ loop xs
    where
      loop : List a -> Maybe (a , a)
      loop xs with (xs)
          | [] | [x] = Nothing
          | [x,y] = Just (x, y)
          | (y :: ys) = loop ys


--leng : List Int -> Eff Int ['ListR ::: STATE (List Int)]
--leng [] = 0
--leng (x :: xs) = do ys <- 'ListR :- get
--                    'ListR :- (put $ tail ys)
--                    leng xs + 1



loopr : List a -> Eff (Maybe a) [STATE Int]
loopr xs with (xs)
    | [] = pure Nothing
    | (y :: ys) = case !get of
                     0 => pure $ Just y
                     t => do put (!get - 1)
                             loopr ys

example : List a -> (n : Int) -> Maybe a
example xs n = runPureInit [n] (loopr xs)

problem3 : Eff (Maybe a) ['ListR ::: STATE (List a), 'Numb ::: STATE Int]
problem3 = do xs <- 'ListR :- get
              y <- 'Numb :- get
              ?pure
              --pure $ example xs y

