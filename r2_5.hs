antiintercalate  :: Eq a => [a] -> [(Int, a)]
antiintercalate  [] = []
antiintercalate  a = [r] ++ antiintercalate(nm) where
  (r, nm) = helper2 (head a) a 0 []

  helper2 x [] k nm1 = ((k, x), nm1)
  helper2 x (ax:as) k nm1 | x == ax = helper2 x as (k+1) nm1
                         | otherwise = ((k, x), (ax:as))