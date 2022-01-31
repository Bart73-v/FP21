module MaybeMonad where

--Daan Eijkman
--Bart Veldman

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f = fmap f

stripMaybe :: Maybe (Maybe a) -> Maybe a
stripMaybe aa = do
    a <- aa
    a

applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
applyMaybe f a = a >>= f