module EditDistance where

--Daan Eijkman
--Bart Veldman

import Data.Array

naiveEditDistance :: String -> String -> Int
naiveEditDistance xs ys = distance xs ys
  where
  distance :: String -> String -> Int
  distance [] ys = length ys
  distance xs [] = length xs
  distance (x:xs) (y:ys) = minimum [1+distance xs (y:ys), 1+distance (x:xs) ys, cost x y+distance xs ys]

  cost x y = if x==y then 0 else 1

editDistance :: String -> String -> Int
editDistance xs ys = distance' (length xs, length ys)
  where
  distance' :: (Int,Int) -> Int
  distance' (x,y) = 

distArray :: Array (Int,Int) Int
distArray = [(ij, !compute! ij) | i <- [0..]]
-- ????

--distArray = array ((0,0), (max_x, max_y)) [(ij, compute ij) | i <- [0..max_x], j <- [0..max_y], let ij = (i,j)]

