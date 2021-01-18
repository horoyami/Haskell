f1_11 :: Int -> Double
f1_11 k | k == 1 = 1
        | k <= 0 = error "ne"
        | otherwise = 1 / (1 + helper 1 (k-1))
  where
    helper _ 0 = 0
    helper m kk = m^(2 :: Int) / (2 + helper (m+2) (kk-1))