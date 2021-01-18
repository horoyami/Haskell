f1_11 :: Int -> Double
f1_11 k | k == 1 = 1
        | k <= 0 = error "ne"
        | otherwise = 1 / (1 + helper 1 (k-1))
  where
    helper m 0 = 0
    helper m k = m^2 / (2 + helper (m+2) (k-1))