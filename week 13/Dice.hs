module Dice where

--Daan Eijkman
--Bart Veldman

import System.Random
import RandomState

data Expr = Lit Integer | Dice Integer 
          | Expr :+: Expr
          | Min Expr Expr | Max Expr Expr
          | Sub Expr Expr | Div Expr Integer
  deriving (Show)

infixl 6 :+: 

type DiceAction m = Integer -> m Integer

--evalM :: Expr ->  DiceAction IO -> IO Integer
--evalM (Lit x)   f = return x
--evalM (Dice x)  f = f x

--evalM (x :+: y) f = do
--  xx <- evalM x f
--  yy <- evalM y f
--  return (xx + yy)

--evalM (Min x y) f = do
--  xx <- evalM x f
--  yy <- evalM y f
--  return (min xx yy)

--evalM (Max x y) f = do
--  xx <- evalM x f
--  yy <- evalM y f
--  return (max xx yy)

evalM :: (Monad m) => Expr -> DiceAction m -> m Integer  -- final version
evalM (Lit x)   f = return x
evalM (Dice x)  f = f x
evalM (x :+: y) f = evalM x f >>= \xx -> evalM y f >>= \yy -> return (xx + yy)
evalM (Min x y) f = evalM x f >>= \xx -> evalM y f >>= \yy -> return (min xx yy)
evalM (Max x y) f = evalM x f >>= \xx -> evalM y f >>= \yy -> return (max xx yy)
evalM (Sub x y) f = evalM x f >>= \xx -> evalM y f >>= \yy -> return (xx - yy)
evalM (Div x y) f = evalM x f >>= \xx -> return (div xx y)

--evalRIO :: Expr -> IO Integer
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= return) -- silent version
--evalRIO expr = evalM expr (\dice->randomRIO (1,dice) >>= report) -- verbose version
--  where report x = do { putStr "rolled a "; print x; return x }

evalIO :: Expr -> IO Integer
evalIO (Lit x) = return x 
evalIO (Dice x) = do
  putStr("Roll 1d" ++ show x ++ ": ")
  input <- getLine
  let n = read input :: Integer
  return n 
evalIO (x :+: y) = evalIO x >>= \xx -> evalIO y >>= \yy -> return (xx + yy)
evalIO (Min x y) = evalIO x >>= \xx -> evalIO y >>= \yy -> return (min xx yy)
evalIO (Max x y) = evalIO x >>= \xx -> evalIO y >>= \yy -> return (max xx yy)
evalIO (Sub x y) = evalIO x >>= \xx -> evalIO y >>= \yy -> return (xx - yy)
evalIO (Div x y) = evalIO x >>= \xx -> return (div xx y)
--evalIO expr = evalM expr (\x -> do {putStr("Roll 1d" ++ show x ++ ": "); input <- getLine; let n = read input :: Integer; return n})


evalND :: Expr -> [Integer]
evalND expr = evalM expr (\k -> [1..k])


avg :: (Fractional a) => [Integer] -> a
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

expectation :: (Fractional a) => Expr -> a
expectation e = avg (evalND e)

-- 12.6.5 ???
{- evalR :: Expr -> RandomState Integer
evalR (Lit x) = return x
evalR _ = put (mkStdGen 5) -}

observed :: (Fractional a) => Int -> Expr -> IO a
observed n expr = observed' n expr >>= \xs -> return (avg xs)

observed' :: Int -> Expr -> IO [Integer]
observed' 0 _ = return []
observed' n expr = evalM expr (\k -> randomRIO (1,k)) >>= \x -> observed' (n-1) expr >>= \xs -> return (x:xs)
