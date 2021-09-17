module Pow2 where

pow2 :: (Ord n, Num n, Num a) => n -> a
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)


-- Maximum n:
-- Integer  -> Infinite
-- Int      -> 2^29-1
-- Float    ->
-- Double   ->