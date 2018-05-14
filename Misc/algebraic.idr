module algebraic

-- Algebraic laws
-- Motivation: Correctness

data Bit = O | I

or : Bit -> Bit -> Bit
or O x1 = x1
or I x1 = I

orAssociative : (a : Bit) ->
                (b : Bit) ->
                (c : Bit) ->
                (a `or` b) `or ` c = a `or` (b `or` c)
orAssociative O b c = refl
orAssociative I b c = refl

BitString : Type
BitString = List Bit
