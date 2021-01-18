apb :: IO ()
apb = do
  a <- getLine
  b <- getLine
  print ((read a :: Int) + (read b :: Int))