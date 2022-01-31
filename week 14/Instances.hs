{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Instances where

import Result

-- you should only implement *either* foldr or foldMap; doing both is unnecessary
instance Foldable Result where
  --foldr :: (a -> b -> b) -> b -> Result a -> b
  foldMap  :: (Monoid m) => (a -> m) -> Result a -> m
  foldMap f (Okay x)  = f x
  foldMap f (Error m) = mempty

instance Traversable Result where
  traverse :: (Applicative f) => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error msg) = pure (Error msg)
  traverse f (Okay x)    = Okay <$> f x

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq,Show)

instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f (Leaf)              = mempty
  foldMap f (Node a (lt) (rt))  = foldMap f lt <> f a <> foldMap f rt

instance Functor Tree where
  fmap :: (a-> b) -> Tree a -> Tree b
  fmap f Leaf               = Leaf
  fmap f (Node a (lt) (rt)) = Node (f a) (fmap f lt) (fmap f rt)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf               = pure Leaf
  traverse g (Node a (lt) (rt)) = pure Node <*> g a <*> traverse g lt <*> traverse g rt


assistants :: Tree String
assistants = Node "Patrick" 
               (Node "Jen" 
                  (Node "Cassian" (Node "Bram" Leaf Leaf) Leaf) 
                  (Node "Mario" Leaf Leaf))
               (Node "Sander" 
                  (Node "Rico" (Node "Quinten" Leaf Leaf) Leaf)
                  (Node "Willem" Leaf Leaf))

flatten :: (Foldable t) => t String  -> String
flatten = foldr1 (\x y->x++", "++y)
