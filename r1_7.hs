f1_7 :: Int -> Int -> Int
f1_7 m n | m < 0 || n < 0 = error "not supported"
         | m == 0 = n+1
         | m > 0 && n == 0 = f1_7 (m-1) 1
         | otherwise = f1_7 (m-1) (f1_7 m (n-1))

f1_8 :: Int -> Int -> Integer
f1_8 m n = toInteger(f1_7 m n)