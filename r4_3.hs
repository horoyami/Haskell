fakeRandom :: IO()
fakeRandom = do
  x <- getLine
  z <- getLine
  let y = read x :: Int
  let k = read z :: Int

  print (getRandomK y k)

getRandomK :: Int -> Int -> [Int]
getRandomK _ 0 = []
getRandomK seed k = [x] ++ (getRandomK x (k-1))
  where
    x = getRandom(seed)

getRandom :: Int -> Int
getRandom seed = x4 + x3 * 2^(16 :: Int) + x2 * 2^(32 :: Int) + x1 * 2^(48 :: Int)
  where
    x1 = getRandom16(seed)
    x2 = getRandom16(x1)
    x3 = getRandom16(x2)
    x4 = getRandom16(x3)

getRandom16 :: Int -> Int
getRandom16 seed = (seed * 1103515245 + 12345) `div` 32768