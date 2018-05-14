module Main

plusAssoc : (l, c, r : Nat) -> l `plus` (c `plus` r) = (l `plus` c) `plus` r
plusAssoc Z     c r = Refl
plusAssoc (S k) c r = rewrite plusAssoc k c r in Refl

plusAssoc2 : (l, c, r : Nat) -> l `plus` (c `plus` r) = (l `plus` c) `plus` r
plusAssoc2 l c r = ?plusAssoc2_rhs

---------- Proofs ----------

Main.plusAssoc2_rhs = proof
  intros
  induction l
  compute
  trivial
  intros
  compute
  rewrite ihn__0
  trivial


