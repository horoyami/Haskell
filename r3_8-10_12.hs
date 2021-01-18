data N = Z | S N deriving (Show, Eq)

instance Ord N where
  compare Z Z = EQ
  compare Z _ = LT
  compare _ Z = GT
  compare (S a) (S b) = compare a b

natSum :: N -> N -> N
natSum a Z = a
natSum a (S b) = natSum (S a) b

natMult :: N -> N -> N
natMult a b = helper Z a b where
  helper r Z _ = r
  helper r (S a1) b1 = helper (natSum r b1) a1 b1

