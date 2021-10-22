module Empty where

{- which ones of these functions are the improper way to test for [] ? -}
-- isEmpty0 :: Foldable t => t a -> Bool
-- isEmpty0 fails for an infinite list of intsand takes very long for many ints in the list.
isEmpty0 list = length list == 0

-- isEmpty1 :: Eq a => [a] -> Bool
-- isEmpty1 doesn't work for a list filled with functions
isEmpty1 list = list == []

-- isEmpty2 :: Foldable t => t a -> Bool
-- Works great
-- This one is prefered because it uses a haskell prelude function
isEmpty2 list = null list

-- isEmpty3 :: [a] -> Bool
-- Works great as wellx
isEmpty3 []   = True
isEmpty3 _    = False

someInts :: [Int]
someInts = [1..32]

manyInts :: [Int]
manyInts = [1..2^(27::Int)]

infiniteInts :: [Int]
infiniteInts = [1..]

nothingAtAll :: [a]
nothingAtAll = []

someFunctions :: [Int->Int->Int]
someFunctions = [(+), (*), mod, div]
