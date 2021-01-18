antiunion :: Eq a => [a] -> [a] -> [a]
antiunion a b = helper1 a b ++ helper1 b a where
  helper1 a1 [] = a1
  helper1 [] _ = []
  helper1 (ax : []) b1 | helper2 ax b1 = [ax]
                      | otherwise = []
  helper1 (ax : as) b1 | helper2 ax b1 = ax : helper1 as b1
                      | otherwise = helper1 as b1
  helper2 _ [] = True
  helper2 x (bx : bs) | x == bx = False
                      | bs == [] = True
                      | otherwise = helper2 x bs