whileNotZero :: IO ()
whileNotZero = do
  x <- whileNotZero2
  print x

whileNotZero2 :: IO Int
whileNotZero2 = do
  x <- getLine
  let y = read x :: Int

  if y == 0
  then return 0
  else do
    s <- whileNotZero2
    return (y + s)