module ListFunctions where

import Prelude hiding (or, and, elem, take, drop)

--Daan Eijkman
--Bart Veldman

and :: [Bool] -> Bool
and []      = True
and (x:xs)  = x &&  and xs

or :: [Bool] -> Bool
or []       = False
or (x:xs)   = x || or xs

elem :: (Eq a) => a -> [a] -> Bool
elem y []       = False 
elem y (x:xs)
  | x == y      = True 
  | otherwise   = elem y xs

drop :: Int -> [a] -> [a]
drop n []       = []
drop 0 xs       = xs
drop n (x:xs)   = drop (n-1) xs

take :: Int -> [a] -> [a]
take n []       = []
take 0 (x:xs)   = []
take n (x:xs)   = x : take (n-1) xs