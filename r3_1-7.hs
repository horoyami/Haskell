import Data.List

data Tree a = Tree {value :: a, left :: (Tree a), mid :: (Tree a), right :: (Tree a)} | Null deriving (Show, Eq)

instance Functor Tree where
  fmap f Null = Null
  fmap f (Tree v l m r) = Tree (f v) (fmap f l) (fmap f m) (fmap f r)

eTree x = Tree x Null Null Null

mergeLists a [] = a
mergeLists [] b = b
mergeLists a (x: bs) = insert x (mergeLists a bs)

treeToList Null = []
treeToList (Tree v l m r) = treeToList(l) ++ treeToList(m) ++ [v] ++ treeToList(r)

sortedListToTree [] = Null
sortedListToTree a = fst (helper a (length a)) where
  helper (x1 : xs) 1 = (eTree x1, xs)
  helper (x1 : x2 : xs) 2 = (Tree x2 (eTree x1) Null Null, xs)
  helper (x1 : x2 : x3 : xs) 3 = (Tree x3 (eTree x1) (eTree x2) Null, xs)
  helper (x1 : x2 : x3 : x4 : xs) 4 = (Tree x3 (eTree x1) (eTree x2) (eTree x4), xs)
  helper a k = ((Tree v l m r), a3) where
    len1 = k `div` 3
    len2 = len1 + (if k `mod` 3 == 2 then 1 else 0)
    len3 = len1 + (if k `mod` 3 /= 0 then 1 else 0)

    (l, a1) = helper a len1
    (m, a2) = helper a1 len2
    v = head a2
    (r, a3) = helper (tail a2) (len3-1)

balance :: Ord a => Tree a -> Tree a
balance t = sortedListToTree (treeToList t)

merge :: Ord a => Tree a -> Tree a -> Tree a
merge t1 t2 = sortedListToTree (mergeLists (treeToList t1) (treeToList t2))

add :: Ord a => a -> Tree a -> Tree a
add x t = merge t (Tree x Null Null Null)

delete :: Ord a => a -> Tree a -> Tree a
delete x t = sortedListToTree (Data.List.delete x (treeToList t))

kmin :: Ord a => Int -> Tree a -> [a]
kmin k t = take k (treeToList t)

seek :: Ord a => a -> Tree a -> Maybe a
seek x Null = Nothing
seek x t | x > value t = seek x (right t)
         | x == value t = Just x
         | x < value t && (left t == Null || x > maxL) = seek x (mid t)
         | otherwise = seek x (left t)
  where
    maxL = maxInTree (left t)
    maxInTree t | right t == Null = value t
                | otherwise = maxInTree (right t)