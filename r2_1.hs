antisort :: Ord a => [a] -> [a]
antisort (a1 : a2 : ax) = shuffle([a1, a2, x3]) ++ rest where
  (x3, rest) = get ax [] a1 a2
  get :: Ord a => [a] -> [a] -> a -> a -> (a, [a])
  get (h : hs) b x1 x2 | x1 == x2 && x1 == h && hs == [] = error "ne"
                       | x1 == x2 && x1 == h = get hs (h : b) x1 x2
                       | otherwise = (h, b ++ hs)
  get [] _ x1 _ = (x1, [])
  shuffle :: Ord a => [a] -> [a]
  shuffle (m1 : m2 : m3 : _) | m1 <= m2 && m2 < m3 || m1 >= m2 && m2 > m3 = [m1, m3, m2]
                             | m1 < m2 && m2 > m3 || m1 > m2 && m2 < m3 = [m1, m2, m3]
                             | m2 == m3 = [m2, m1, m3]
  shuffle _ = []
antisort _ = []