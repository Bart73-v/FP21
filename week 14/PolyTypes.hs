module PolyTypes where

-- :: (a -> b) -> a -> b
lift0 f x          = f x
-- :: (b -> c) -> (a -> b) -> a -> c
lift1 f g1 x       = f (g1 x)
-- :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
lift2 f g1 g2 x    = f (g1 x) (g2 x)

deMorgan quantor p   = not . quantor (not . p)
