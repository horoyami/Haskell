f1_9 :: Double -> Double -> Double -> Double -> (Double, Double, Double)
f1_9 a b c d | s <= 0 = error "nea"
             | otherwise = (x1, x2, x3)
  where
    na = 1
    nb = b/a
    nc = c/a
    nd = d/a
    q = (nb^2 - 3*nc) / 9
    r = (2*nb^3 - 9*nb*nc + 27*nd) / 54
    s = q^3 - r^2
    fi = 1/3 * acos (r / sqrt (q^3))
    x1 = -2 * sqrt q * cos fi - nb/3
    x2 = -2 * sqrt q * cos (fi - 2/3*pi) - nb/3
    x3 = -2 * sqrt q * cos (fi + 2/3*pi) - nb/3