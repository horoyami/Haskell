antiprimes :: Int -> [Integer]
antiprimes n = take n (diffList [1..lim] (createPrimes lim)) where
  lim = toInteger n * 2

  createPrimes :: Integer -> [Integer]
  createPrimes k = helper [2..k] where
     helper []     = []
     helper (p:xs) = p : helper (diffList xs [p^(2 :: Int), p^(2 :: Int)+p..k])

  diffList :: [Integer] -> [Integer] -> [Integer]
  diffList [] _ = []
  diffList xs [] = xs
  diffList (x:xs) (y:ys) | x < y = x : diffList xs (y:ys)
                         | x > y = diffList (x:xs) ys
                         | otherwise = diffList xs ys
