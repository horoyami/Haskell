import Text.Read

getNumberOrNot :: String -> Maybe Integer
getNumberOrNot s = readMaybe (filter (\c -> c /= ' ' && c /= '\t' && c /= '\n') s)