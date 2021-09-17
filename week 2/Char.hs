module Char where

import Data.Char

(~~) :: String -> String -> Bool
[]      ~~ []     = True 
[]      ~~ _      = False
_       ~~ []     = False
(x:xs)  ~~ (y:ys) = (toLower x == toLower y) && (xs ~~ ys)

reverseCase :: String -> String
reverseCase []      = []
reverseCase (x:xs)  = if isUpper x  then toLower x : reverseCase xs 
                                    else toUpper x : reverseCase xs

--shift :: Int -> Char -> Char

--caesar :: Int -> String -> String

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"
