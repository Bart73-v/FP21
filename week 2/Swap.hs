module Swap where

--swap :: (Int, Int) -> (Int, Int)
swap (x, y) = (y, x)

--squareTuple :: Int, Int) -> (Int, Int)
squareTuple (x, y) = (x^2, y^2)

--modulo10Tuple :: Int, Int) -> (Int, Int)
modulo10Tuple (x, y) = (x `mod` 10, y `mod` 10)

subExercise4 :: (Int, (Char, Bool)) -> (Int, Char, Bool)
subExercise4 (a, (b, c)) = (a, b, c)

-- 1.2 Making squareTuple and modulo10Tuple of type (a, b) -> (b, a) doesn't work
-- 1.3 swap has type (t1, t) -> (t, t1)
--     squareTuple has type (Num t1, Num t) => (t1, t) -> (t1, t)
--     modulp10Tuple has type (Integral t1, Integral t) => (t1, t) -> (t1, t)
-- 1.4 The difference is that the first data format is a tuple with two elements of which one is a tuple of (Char, Bool) and the second data format is a tuple with three elements without another tuple embedded.
