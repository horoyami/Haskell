import Data.List.Split
import Data.List
import Data.Char

getMailDomainOrNot :: String -> Maybe String
getMailDomainOrNot ss = checkEmail ss 0 where
  checkEmail [] _ = Nothing
  checkEmail (c:s) z | z == 0 && check0 c = checkEmail s 1
                     | (z == 1 || z == 2) && c == '@' = checkEmail s 3
                     | z == 1 && check1 c = checkEmail s 0
                     | z == 1 && check2 c = checkEmail s 1
                     | z == 2 && check0 c = checkEmail s 1
                     | z == 3 && check3 c && c == '.' = checkEmail s 4
                     | z == 3 && check3 c = checkEmail s 3
                     | z == 4 && check4 c = checkEmail s 5
                     | z == 5 && check4 c = if (s == []) then genDomain else checkEmail s 5
                     | otherwise = Nothing
  check0 c = check4 c || isDigit c
  check1 c = c == '.' || c == '-' || c == '_'
  check2 c = check0 c || c == '@'
  check3 c = check0 c || c == '-' || c == '.'
  check4 c = c >= 'a' && c <= 'z' || c >='A' && c <= 'Z'
  genDomain = Just ((head . tail) (splitOn "@" ss))

test = [
  getMailDomainOrNot "abc-d@mail.com" == Just "mail.com",
  getMailDomainOrNot "abc.def@mail.com" == Just "mail.com",
  getMailDomainOrNot "abc@mail.com" == Just "mail.com",
  getMailDomainOrNot "abc_def@mail.com" == Just "mail.com",
  getMailDomainOrNot "abc-@mail.com" == Nothing,
  getMailDomainOrNot "abc..def@mail.com" == Nothing,
  getMailDomainOrNot ".abc@mail.com" == Nothing,
  getMailDomainOrNot "abc#def@mail.com" == Nothing,
  getMailDomainOrNot "abc.def@mail.cc" == Just "mail.cc",
  getMailDomainOrNot "abc.def@mail-archive.com" == Just "mail-archive.com",
  getMailDomainOrNot "abc.def@mail.org" == Just "mail.org",
  getMailDomainOrNot "abc.def@mail.com" == Just "mail.com",
  getMailDomainOrNot "abc.def@mail.c" == Nothing,
  getMailDomainOrNot "abc.def@mail#archive.com" == Nothing,
  getMailDomainOrNot "abc.def@mail" == Nothing,
  getMailDomainOrNot "abc.def@mail..com" == Nothing,
  getMailDomainOrNot "bad-adress@mail.r" == Nothing,
  getMailDomainOrNot "super-bad-adress-lol@gmail.com" == Just "gmail.com"]