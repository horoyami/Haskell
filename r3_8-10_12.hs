data N = Z | S N deriving (Show, Eq)

instance Ord N where
  compare Z Z = EQ
  compare Z b = LT
  compare a Z = GT
  compare (S a) (S b) = compare a b

natSum :: N -> N -> N
natSum a Z = a
natSum a (S b) = natSum (S a) b

natMult :: N -> N -> N
natMult a b = helper Z a b where
  helper r Z b = r
  helper r (S a) b = helper (natSum r b) a b

