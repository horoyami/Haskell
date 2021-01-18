antiunion :: Eq a => [a] -> [a] -> [a]
antiunion a b = helper1 a b ++ helper1 b a where
  helper1 a [] = a
  helper1 [] b = []
  helper1 (ax : []) b | helper2 ax b = [ax]
                      | otherwise = []
  helper1 (ax : as) b | helper2 ax b = ax : helper1 as b
                      | otherwise = helper1 as b
  helper2 x (bx : bs) | x == bx = False
                      | bs == [] = True
                      | otherwise = helper2 x bs