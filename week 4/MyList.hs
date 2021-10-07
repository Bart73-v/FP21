module MyList where

  --Daan Eijkman
--Bart Veldman

data MyList a = a :# MyList a | Null
  deriving (Eq,Ord)
