module Unfold where

import Data.List (unfoldr)
import Prelude hiding (take,zip,(++))
--Daan Eijkman
--Bart Veldman

bits :: Int -> [Int]
bits x = unfoldr (\n -> if 2^n > x then Nothing else Just(if x `mod` 2^(n+1) >= 2^n then 1 else 0, n+1)) 0

zip :: [a] -> [b] -> [(a,b)]
zip xs ys = unfoldr (\n -> if (n >= length xs || n >= length ys) then Nothing else Just((xs !! n, ys !! n), n+1)) 0 

take :: Int -> [a] -> [a]
take m xs = unfoldr (\n -> if (n >= length xs || n >= m) then Nothing else Just(xs !! n, n+1)) 0

--primes :: [Integer]
-- ??

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo f seed = case f seed of
               Left l       -> l
               Right (a,ns) -> a : apo f ns

--(++) :: [a] -> [a] -> [a]
--xs ++ ys = apo () 

-- insert :: (Ord a) => a -> [a] -> [a]
-- insert x xs = apo ( ) 0

-- unfoldrApo :: (t -> Maybe (a, t)) -> t -> [a]
