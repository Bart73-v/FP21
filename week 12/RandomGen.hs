module RandomGen where

--Daan Eijkman
--Bart Veldman

import Control.Monad
import System.Random
import RandomState

genRandIntegerIO :: (Integer,Integer) -> IO Integer
genRandIntegerIO (n,m) = do
  gen <- getStdGen
  let (x, gen') = randomR (n,m) gen
  return x

genRandInteger :: (Integer,Integer) -> RandomState Integer
genRandInteger (a,b) = do
  gen <- get
  let (x, gen') = randomR (a,b) gen
  return x

roll_2d6 :: RandomState Integer
roll_2d6 = do
  a <- genRandInteger (1,6)
  b <- genRandInteger (1,6)
  return (a+b)

--safeR :: RandomState a -> IO a  ????
--safeR m = do
--  aa <- get a
--  let gen = mkStdGen 2
--  setStdGen gen


--these definitions can be used to test your function a bit more thoroughly
randomN :: (Integer,Integer) -> Int -> StdGen -> [Integer]
randomN (a,b) n g = result
  where (result, _) = runState (replicateM n (genRandInteger (a,b))) g

testme :: [Integer]
testme = randomN (0,999) 100 (mkStdGen 42)
