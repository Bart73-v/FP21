module Lego where

import Data.List
import Data.Tuple

--Daan Eijkman
--Bart Veldman

removeAt :: Int -> [a] -> [a]
removeAt n xs = [ x | (i,x) <- zip [1..] xs, i /= n]

sortWithPos :: (Ord a) => [a] -> [(a,Int)]
sortWithPos xs = sortOn fst (zip xs [0..])

sortedPos :: (Ord a) => [a] -> [(a,Int)]
sortedPos xs = [(b,c) | ((a,b),c) <- (sortOn (fst . fst) (zip (sortOn snd (zip [0..] xs)) [0..]))]