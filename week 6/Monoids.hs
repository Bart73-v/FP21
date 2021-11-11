module Monoids where

--Daan Eijkman
--Bart Veldman

{- Determine if the following operations turn a type into a Monoid;
 - if so: give the corresponding value for the identity element

   The list cons operator,   (:) :: a -> [a] -> [a]
   not associative

   The boolean operator      (||) :: Bool -> Bool -> Bool
   mempty = False

   The function              mod :: (Integral a) => a -> a -> a
   not associative

   The function              max :: (Ord a) => a -> a -> a

   Function composition,     (.) :: (b -> c) -> (a -> b) -> (a -> c)

   The function              zipWith (+) :: [Int] -> [Int] -> [Int]

   The operator              (++/) :: [a] -> [a] -> [a] defined below
  -}

(++/) :: [a] -> [a] -> [a]
[]     ++/ ys = ys
(x:xs) ++/ ys = x:(ys ++/ xs)
