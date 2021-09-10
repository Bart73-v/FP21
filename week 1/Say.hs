module Say where

say :: Integer -> String
say n
  | n > 999   = say (n `div` 1000) ++ " thousand" ++ spaces n 1000 ++ say (n `mod` 1000)
  | n > 99    = say (n `div` 100) ++ " hundred" ++ spaces n 100 ++ say (n `mod` 100)
  | n > 19    = decimals (n `div` 10) ++ spaces n 10 ++ say (n `mod` 10)
  | n == 19   = "nineteen"
  | n == 18   = "eighteen"
  | n == 17   = "seventeen"
  | n == 16   = "sixteen"
  | n == 15   = "fifteen"
  | n == 14   = "fourteen"
  | n == 13   = "thirteen"
  | n == 12   = "twelve"
  | n == 11   = "eleven"
  | n == 10   = "ten"
  | n == 9    = "nine"
  | n == 8    = "eight"
  | n == 7    = "seven"
  | n == 6    = "six"
  | n == 5    = "five"
  | n == 4    = "four"
  | n == 3    = "three"
  | n == 2    = "two"
  | n == 1    = "one"
  | otherwise = ""
  where spaces n 100 = if n `mod` 100 == 0 then "" else " and "
        spaces n m   = if   n `mod` m == 0 then "" else " "

decimals :: Integer -> String 
decimals 2 = "twenty"
decimals 3 = "thirty"
decimals 4 = "forty"
decimals 5 = "fifty"
decimals 6 = "sixty"
decimals 7 = "seventy"
decimals 8 = "eighty"
decimals 9 = "ninety"
decimals _ = ""

