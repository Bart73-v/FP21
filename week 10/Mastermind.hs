module Main where

--Daan Eijkman
--Bart Veldman

import Data.List
import Data.Char
import Data.Maybe
import System.Random
import System.IO

{---------------------- functional parts -------------------------------}

data Colour = White | Silver | Green | Red | Orange | Pink | Yellow | Blue
  deriving (Show, Enum, Bounded, Read, Ord, Eq)

scoreAttempt :: (Ord a) => [a] -> [a] -> (Int, Int)
scoreAttempt code guess = (a, b-a)
  where a = rightPos code guess
        b = wrongPos code guess

rightPos :: (Ord a) => [a] -> [a] -> Int
rightPos code guess = length $ filter (\x -> x) [a == b | (a,b) <- zip code guess]

wrongPos :: (Ord a) => [a] -> [a] -> Int
wrongPos code guess = length $ filter (\x -> x) [ a == b | a <- guess', b <- code']
  where guess' = map head (group (sort guess))
        code' = map head (group (sort code))

-- Some test cases from: https://www.boardgamecapital.com/game_rules/mastermind.pdf
test1, test2, test3, test4 :: Bool
test1 = scoreAttempt [1,2,3,4,5 :: Int] [2,6,3,7,8 :: Int] == (1,1)
test2 = scoreAttempt [1,2,3,4,2 :: Int] [5,6,3,3,7 :: Int] == (1,0)
test3 = scoreAttempt [1,2,1,3,3 :: Int] [4,1,5,6,7 :: Int] == (0,1)
test4 = scoreAttempt [4,1,5,6,7 :: Int] [1,2,1,3,3 :: Int] == (0,1)

{---------------------- IO parts -------------------------------}

-- only here for example; you can remove these from your final file
roll_d6 :: IO Int
roll_d6 = randomRIO (1,6)

roll_2d6 :: IO Int
roll_2d6 = do
  a <- roll_d6
  b <- roll_d6
  return (a + b)


allColours = [(minBound :: Colour) ..]

getCode :: Int -> IO [Colour]
getCode n
  | n > 0     = do
              i <- randomRIO (0, length allColours - 1)
              c <- return $ allColours !! i
              cs <- getCode (n-1)
              return $ [c] ++ cs
  | otherwise = return []

playGame :: Int -> [Colour] -> IO ()
playGame 0 code = putStrLn ("No more tries, game over.\n The code was " ++ show code)
playGame n code = do
    putStrLn "Please enter your guess:"
    guess' <- getLine
    let guess = [ b | a <- (words guess'), b <- allColours, (toLower (head a)) == (toLower (head (show b)))]
    let (a,b) = scoreAttempt code guess
    if a == 4 then do
      putStr "Correct" 
      return ()
    else do
      putStr ("Incorrect\n" ++ show a ++ " colour(s) in the correct position,\n" ++ show b ++ " colour(s) in the wrong position.\n")
      playGame (n-1) code
      return ()


main :: IO ()
main = do
  putStrLn "How long should the secret code be?"
  input <- getLine
  let n = read input :: Int 
  code <- getCode n 
  putStrLn "How many tries?"
  input <- getLine 
  let m = read input :: Int
  playGame m code 
  return ()
