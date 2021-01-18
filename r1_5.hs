isPrime :: Int -> Bool
isPrime x = isPrimeH (abs x) 2 where
  sqrX = (floor . sqrt . fromIntegral . abs) x + 1
  isPrimeH x i | x == 1 || mod x i == 0 = False
              | i == sqrX = True
              | otherwise = isPrimeH x (i+1)

f1_5 :: Int -> Int
f1_5 x = helper 2 where
  helper i | isPrime(mersen) && perfect > x = perfect
             | otherwise = helper (i+1)
    where
      mersen = 2^i - 1
      perfect = 2^(i-1) * mersen