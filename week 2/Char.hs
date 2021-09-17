module Char where

--Daan Eijkman
--Bart Veldman

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

shift :: Int -> Char -> Char
shift n c = if ord c < 91 && ord c > 64 then chr ((n + (ord c-65)) `mod` 26 + 65) else c

caesar :: Int -> String -> String
caesar n = map (shift n . toUpper)

msg :: String
msg = "ADMNO D HPNO NKMDIFGZ TJP RDOC AVDMT YPNO"

fairyDust = caesar 5 msg
