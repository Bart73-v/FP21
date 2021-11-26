module Stream where

--Daan Eijkman
--Bart Veldman

import qualified Data.List as List
import Prelude hiding (head, tail, repeat, map, zipWith, filter, take, drop, concat, cycle, sum)

data Stream a = a :> Stream a
infixr 5 :>

instance (Show a) => Show (Stream a) where
  show s = "(" List.++ showN (16::Int) s List.++ ")"
    where
    showN 0 _         = "..."
    showN n (x :> xs) = show x List.++ " :> " List.++ showN (n-1) xs

from :: Integer -> Stream Integer
from n = n :> from (n + 1)

head :: Stream a -> a
head (x :> y) = x

tail :: Stream a -> Stream a
tail (x :> y) = y

repeat :: a -> Stream a
repeat x = x :> repeat x

map :: (a -> b) -> (Stream a -> Stream b)
map f (x :>  y) = f x :> map f y

zipWith :: (a -> b -> c) -> (Stream a -> Stream b -> Stream c)
zipWith f (x :> xs) (y :> ys) = (f x y) :> zipWith f xs ys  

filter :: (a -> Bool) -> Stream a -> Stream a
filter f (x :> xs)
  | f x == True   = x :> filter f xs
  | f x == False  = filter f xs

-- 9.4.3
-- filter (\x->False) (from 0) will try to filter out each element from the Stream since each element equals False.
-- The implementation of show shows the first 16 elements, however this function will never get to 16 elements and since 
-- a Stream is infinite this function will simply hang

toList :: Stream a -> [a]
toList (x :> xs) = [x] ++ toList xs

cycle :: [a] -> Stream a
cycle (x:xs) = cycle' (x:xs) (x:xs)

cycle' :: [a] -> [a] -> Stream a
cycle' []     ys  = cycle' ys ys
cycle' (x:xs) ys  = x :> cycle' xs ys

nat, fib :: Stream Integer
nat = 0 :> zipWith (+) nat (repeat 1)
fib = 0 :> 1 :> zipWith (+) fib (tail fib)

primetwins :: Stream (Integer,Integer)
primetwins = primetwins' prime

primetwins' :: Stream Integer -> Stream (Integer,Integer)
primetwins' (x :> y :> ys)
  | y-x <= 2  = (x,y) :> primetwins' (y :> ys)
  | otherwise = primetwins' (y :> ys)

prime :: Stream Integer
prime = filter isPrime nat

isPrime p = foldl (\acc x -> acc * x `mod` p) 1 [2..(p-2)] == 1

combine :: Stream a -> Stream a -> Stream a
combine (x :> xs) (y :> ys) = x :> y :> combine xs ys
