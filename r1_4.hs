f1_4 :: Int -> Int
f1_4 x = if x /= 0 then isPrime (abs x) 2 1 else 0 where
  sqrX = (floor . sqrt . fromIntegral . abs) x + 1
  isPrime x i s | i == sqrX = s
                | i ^ 2 == x = s + sqrX - 1
                | mod x i == 0 = isPrime x (i+1) s+i+(div x i)
                | otherwise = isPrime x (i+1) s
