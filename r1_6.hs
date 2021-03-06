isPrime :: Integer -> Bool
isPrime n = isPrimeH (abs n) 2 where
  sqrX = floor(sqrt(fromIntegral(abs(n))) :: Double) + 1
  isPrimeH x i | x == 1 || mod x i == 0 = False
              | i == sqrX = True
              | otherwise = isPrimeH x (i+1)

f1_6 :: Integer -> Integer
f1_6 x = helper 2 where
  helper :: Integer -> Integer
  helper i | isPrime(mersen) && perfect > x = perfect
             | otherwise = helper (i+1)
    where
      mersen = 2^i - 1
      perfect = 2^(i-1) * mersen