module Replicate where

--Daan Eijkman
--Bart Veldman

-- this is the definition in the slides:
replicateM' :: (Monad m) => Int -> m a -> m [a]
replicateM' 0 _  = return []
replicateM' n mx = (:) <$> mx <*> replicateM' (n-1) mx

replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM 0 _  = return []
replicateM n mx = do
  x <- mx
  xs <- replicateM (n-1) mx
  return  (x:xs)

--e1 :: IO String       -> Concatenate input 4 times, inside IO (wrong)
--e2 :: Maybe a         -> Nothing
--e3 :: Maybe Int       -> Just [37,37,37,37]
--e4 :: List            -> [0,0,0,0,1,1,1,1] (wrong)