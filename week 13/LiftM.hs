module LiftM where

--Daan Eijkman
--Bart Veldman

-- implement without 'fmap', '<$>' or '<*>'
liftM :: (Monad m) => (a -> b) -> m a -> m b
--liftM f mx = fmap f mx
liftM f mx = mx >>= \x -> return (f x)
