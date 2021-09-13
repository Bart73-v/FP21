--Daan Eijkman
--Bart Veldman

module Hello where

import Say (say, decimals)
  
lyrics :: Integer -> String
lyrics 0 = "no more seats in the lecture hall!\n" 
lyrics n = say n ++ " " ++ seats ++ " in the lecture hall! " ++
           "Only " ++ say n ++ " " ++ seats ++ " left!\n" ++
           "A students walks in, and sits down, now there " ++ areis ++ "\n" ++
           lyrics (n-1)
  where seats = if n /= 1 then "seats" else "seat"
        areis = if (n-1) == 1 then "is" else "are"
  
song :: String
song = lyrics 75
