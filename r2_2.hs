antiprimes :: Int -> [Integer]
antiprimes n = take n (diffList [1..lim] (createPrimes lim)) where
  lim = toInteger n * 2

  createPrimes :: Integer -> [Integer]
  createPrimes n = helper [2..n] where
     helper []     = []
     helper (p:xs) = p : helper (diffList xs [p^2, p^2+p..n])

  diffList :: [Integer] -> [Integer] -> [Integer]
  diffList [] _ = []
  diffList xs [] = xs
  diffList (x:xs) (y:ys) | x < y = x : diffList xs (y:ys)
                         | x > y = diffList (x:xs) ys
                         | otherwise = diffList xs ys
