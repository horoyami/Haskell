f1_4 :: Int -> Int
f1_4 n = if n /= 0 then isPrime (abs n) 2 1 else 0 where
  sqrX = floor(sqrt(fromIntegral(abs(n))) :: Double) + 1
  isPrime x i s | i == sqrX = s
                | (i ^ (2 :: Int)) == x = s + sqrX - 1
                | mod x i == 0 = isPrime x (i+1) s+i+(div x i)
                | otherwise = isPrime x (i+1) s
