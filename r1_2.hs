f1_2 :: Int -> Bool
f1_2 n = isPrime (abs n) 2 where
  sqrX = floor(sqrt(fromIntegral(abs(n))) :: Double) + 1
  isPrime x i | x == 1 || mod x i == 0 = False
              | i == sqrX = True
              | otherwise = isPrime x (i+1)