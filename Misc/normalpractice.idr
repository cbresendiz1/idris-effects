module Pracitce

last : List a -> Maybe a
last xs with (xs)
  | [] = Nothing
  | (x :: ys) = last ys
  | [x] = Just x

last_tow : List a -> Maybe (a, a)
last_tow xs with (xs)
  | [] | [x] = Nothing
  | [x, y] = Just (x, y)
  | (x :: ys) = last_tow ys

at : List a -> Integer -> Maybe a
at xs n with (xs)
  | [] = Nothing
  | (x :: ys) = if n == 0 then Just x else at ys (n - 1)



length : List a -> Integer
length xs with (xs)
  | [] = 0
  | (y :: ys) = length ys + 1

reverse2 : List a -> List a
reverse2 xs = rev xs []
  where
    rev : List a -> List a -> List a
    rev xs aux with (xs)
      | [] = aux
      | (y :: ys) = rev ys (y :: aux)

palindrome : (Eq a) => List a -> Bool
palindrome xs = xs == reverse2 xs

data Node a = One a
            | Many (List (Node a))

--- Need to be reworked

flatten : List (Node a) -> List a
flatten xs with (xs)
  | [] = []
  | (One y :: ys) = y :: (flatten ys)

data BTree a = Leaf
             | Noder (BTree a) a (BTree a)

testTree : BTree String
testTree = Noder (Noder Leaf "Jordon" Leaf)
                "Fred"
		(Noder (Noder Leaf "Alice" Leaf)
		       "Sheila"
		       (Noder Leaf "Bob" Leaf))

