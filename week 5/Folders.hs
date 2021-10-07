module Folders where

import Prelude hiding (and,or,elem,maximum)
--Daan Eijkman
--Bart Veldman

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

elem :: (Eq a) => a -> [a] -> Bool
elem a = foldr (\x ys -> a==x || ys) False

--maximum :: (Ord a) => [a] -> a
--maximum = foldr (\x y -> if x>y then x else y) ???

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr (\x y -> insert x y) Leaf

fromBits :: [Integer] -> Integer
fromBits = foldr (\a n -> a + n * 2) 0

{- -------------------------------------------------------------------}

-- the relevant definitions for 'fromList'

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x tree@(Node key lt rt)
  | x < key   = Node key (insert x lt) rt
  | x > key   = Node key lt (insert x rt)
  | otherwise = tree
