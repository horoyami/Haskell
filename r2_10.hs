import Data.Maybe

stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)]
stupidTraverse a | len `mod` 4 == 0 && len /= 0 = Just (helper clearA)
                 | otherwise = Nothing
  where
  clearA = filter (\x -> not $ isNothing x) a
  len = length clearA

  helper [] = []
  helper ((Just x1) : (Just x2) : (Just x3) : (Just x4) : s) = [(x1,x2,x3,x4)] ++ (helper s)