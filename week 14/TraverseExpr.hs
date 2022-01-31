{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

--Daan Eijkman
--Bart Veldman

import Control.Monad.State
import Data.Foldable
import Data.List
import Data.Maybe

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  fmap :: (a -> b) -> (Expr a -> Expr b)
  fmap f (Var x) = Var (f x)
  fmap _ (Lit x) = Lit x
  fmap f (Op x le re) = Op x (fmap f le) (fmap f re)

instance Foldable Expr where
  foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Var x) = f x
  foldMap _ (Lit _) = mempty
  foldMap f (Op _ le re) = (foldMap f le) <> (foldMap f re)

instance Traversable Expr where
  traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse g (Var x) = Var <$> g x
  --traverse g (Lit x) = pure ()
  traverse g (Op x le re) = (Op x) <$> traverse g le <*> traverse g re

allVars :: (Ord a) => Expr a -> [a]
allVars = map head . group . sort . foldr (:) []

renameVar :: String -> State [(String,Int)] Int
renameVar name = do 
  vars <- get
  if any (\(x,y) -> x == name) vars then do 
    let xs = [y | (x,y) <- vars, x == name]
    return (head xs)
  else do
    let vars' = vars ++ [(name,2)]
    put vars'
    return 2
{- 
get :: State s s
put :: s -> State s ()
return :: a -> State s a
-}

indexVars :: Expr String -> Expr Int
indexVars expr = do
  let s = renameAllVars expr
  evalState s [("x",0),("y",1)]

renameAllVars :: Expr String -> State [(String,Int)] (Expr Int)
renameAllVars expr = traverse renameVar expr
