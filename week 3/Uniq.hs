module Uniq where

--Daan Eijkman
--Bart Veldman

uniq :: (Eq a) => [a] -> [a]
uniq (x:y:zs)
    | x == y    = x : uniq zs
    | otherwise = x : uniq (y:zs)
uniq (x:xs) = [x]
uniq []     = []
