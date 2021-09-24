module ListComprehensions where

import Data.List

--Daan Eijkman
--Bart Veldman

-- Creates a list of all possible combinations of elements from as and bs
combinations :: [a] -> [b] -> [(a,b)]
combinations as bs = [ (a,b) | a <- as, b <- bs ]

-- Creates a list containing n times element y
-- Overloaded, Num n
repeat' :: (Num a, Enum a) => a -> b -> [b]
repeat' n y   = [ y | i <- [1..n] ]

-- Take n elements from xs
take' :: (Num a, Enum a, Ord a) => a -> [b] -> [b]
take' n xs  = [ x | (i,x) <- zip [0..] xs, i < n ]

-- Get the indices of element a in list xs
elemIndices' :: (Eq a) => a -> [a] -> [Integer]
elemIndices' a xs  = [ i | (i,x) <- zip [0..] xs, x == a]

-- Zip but instead of tuples create a list of all elements
zipList :: [a] -> [a] -> [a]
zipList xs ys = [ e | (x,y) <- zip xs ys, e <- [x,y] ]

-- Concatenate all elements
concat' :: [[a]] -> [a]
concat' xss   = [ x | xs <- xss, x <- xs ]