module Monoids where
import Data.Monoid

--Daan Eijkman
--Bart Veldman


-- && || XOR    Fourth one???

reduce :: (Monoid m) => [m] -> m
reduce = foldl (<>) mempty

newtype AndBool = And { fromAnd :: Bool }
    deriving (Show)
newtype OrBool  = Or  { fromOr  :: Bool }
    deriving (Show)
newtype XorBool = Xor { fromXor :: Bool }
    deriving (Show)
--newtype NorBool = Nor { fromNor :: Bool }
--    deriving (Show)

instance Semigroup AndBool where
    x <> y = And (fromAnd x && fromAnd y)
instance Monoid AndBool where
    mempty = And True
-- returns  And {fromAnd = True} if all And's are True
--          And {fromAnd = False} otherwise

instance Semigroup OrBool where
    x <> y = Or (fromOr x || fromOr y)
instance Monoid OrBool where
    mempty = Or False
-- returns Or {fromOr = True} if one or more of the Or's is True
--         Or {fromOr = False} otherwise

instance Semigroup XorBool where
    x <> y = Xor ((u || v) && not (u && v)) 
        where u = fromXor x
              v = fromXor y
instance Monoid XorBool where
    mempty = Xor False
-- returns Xor {fromXor = True} if nr. of Xor's being True is odd
--         Xor {fromXor = False} otherwise

--instance Semigroup NorBool where
--    x <> y = Nor (not (u || v))
--        where u = fromNor x 
--              v = fromNor y
--instance Monoid NorBool where
--    mempty = Nor False
-- We realized Nor is not a monoid, but can not figure out what other Bool -> Bool -> Bool function is a monoid
