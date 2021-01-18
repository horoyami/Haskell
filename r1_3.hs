f1_3 :: Bool -> Bool -> Int
f1_3 a b | a && b = 2
         | a || b = 1
         | otherwise = 0