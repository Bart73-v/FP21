module Pow2 where

--Daan Eijkman
--Bart Veldman

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)

-- Maximum n:
-- Integer  -> Infinite
-- Int      -> 2^29-1
-- Float    -> We can't find the bound of floats
-- Double   -> Infinite