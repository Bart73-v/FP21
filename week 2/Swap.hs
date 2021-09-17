module Swap where

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

squareTuple :: (a, b) -> (b, a)
squareTuple (x, y) = (x^2, y^2)

modulo10Tuple :: (a, b) -> (b, a)
modulo10Tuple (x, y) = (x `mod` 10, y `mod` 10)

-- Making squareTuple and modulo10Tuple of type (a, b) -> (b, a) doesn't work
