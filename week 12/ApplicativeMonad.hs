module ApplicativeMonad where

--Daan Eijkman
--Bart Veldman

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just x) (Just y) = Just (f x y)
liftMaybe2 _ _ _ = Nothing

liftA2 :: (Applicative m) => (a -> b -> c) -> m a -> m b -> m c
liftA2 f mx my = pure f <*> mx <*> my

liftM2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = do
  x <- mx
  y <- my
  return $ f x y

liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f mx my = mx >>= \x -> my >>= \y -> return (f x y)

expr1 = liftM2 (++) getLine (return "!")

expr2 = liftM2 (*) (Just 6) (Just 7)

expr3 = liftM2 take [1..10] [fib]
  where fib  = 0 : fib'
        fib' = 1 : zipWith (+) fib fib'
