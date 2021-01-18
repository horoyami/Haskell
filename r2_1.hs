antisort :: Ord a => [a] -> [a]
antisort (a1 : a2 : ax) = shuffle([x1, x2, x3]) ++ rest where
  x1 = a1
  x2 = a2
  (x3, rest) = get ax []
  get (h : hs) b | x1 == x2 && x1 == h && hs == [] = error "ne"
                 | x1 == x2 && x1 == h = get hs (h : b)
                 | otherwise = (h, b ++ hs)
  shuffle (m1 : m2 : m3 : _) | m1 <= m2 && m2 < m3 || m1 >= m2 && m2 > m3 = [m1, m3, m2]
                             | m1 < m2 && m2 > m3 || m1 > m2 && m2 < m3 = [m1, m2, m3]
                             | m2 == m3 = [m2, m1, m3]