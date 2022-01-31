module Result where

--Daan Eijkman
--Bart Veldman

import Data.List

data Result a = Okay a | Error [String]
  deriving (Eq,Ord,Show)

instance Functor Result where
--fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error s)  = Error s
  fmap f (Okay a)   = Okay (f a)

instance Applicative Result where
-- pure :: a -> Result a
  pure x = Okay x
-- (<*>) :: Result (a -> b) -> Result a -> Result b
  (Error s) <*> (Error s')  = Error (s ++ s')
  (Error s) <*> _           = Error s
  (Okay f) <*> x            = fmap f x