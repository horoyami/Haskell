antimerge :: Eq a => [a] -> [(Int, a)]
antimerge [] = []
antimerge a = [r] ++ antimerge(nm) where
  (r, nm) = helper2 (head a) a 0 []

  helper2 x [] k nm1 = ((k, x), nm1)
  helper2 x (ax:as) k nm1 | x == ax = helper2 x as (k+1) nm1
                         | otherwise = helper2 x as k (ax:nm1)