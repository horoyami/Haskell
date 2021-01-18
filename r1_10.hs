f1_10 :: Double -> Double -> (Double, Double)
f1_10 x y | y == 0 = error "zero"
          | otherwise = (q, m)
    where
      q = realToFrac $ floor (x/y)
      m = x - q * y