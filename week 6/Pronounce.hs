module Pronounceable where

import Data.List
import Say (say)

class Pronounceable a where
  pronounce :: a -> String

instance Pronounceable Char where
  pronounce c = unwords ["the","character", "'"++[c]++"'"]

instance Pronounceable a => Pronounceable [a] where
  pronounce xs = "a list containing " ++ (intercalate ", " $ map pronounce xs)
  
instance Pronounceable Integer where
  pronounce = say . toInteger

instance Pronounceable Int where
  pronounce = say . toInteger
  
-- A little bit hardcoded
-- The idea: truncate i ++ " point " ++ round((i - truncate(i))*10)
instance Pronounceable Double where
  pronounce i = pronounce(truncate(i) :: Integer) ++  " point " ++ rounding(i)
          where rounding :: Double -> String
                rounding i = pronounce(round((i - (fromIntegral . truncate) i)*10) :: Integer)

instance (Pronounceable a, Pronounceable b) => Pronounceable (a, b) where
  pronounce (x, y) = "A tuple containing" ++ pronounce x ++ " and " ++ pronounce y
 
data FuncWithArg a b = (a -> b) :$: a
 
instance (Pronounceable a, Pronounceable b) => Pronounceable (FuncWithArg a b) where
  pronounce (f :$: x) = "A function from " ++ pronounce x ++ " to " ++ pronounce (f x)
