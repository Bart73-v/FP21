-- ghc --make WordCount.hs
module Main where

--Daan Eijkman
--Bart Veldman

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  filecontents <- mapM readFile args
  output1 <- return $ map wcCount filecontents
  output2 <- return $ zip output1 args
  print output2
  --print args
  --or, to get "UNIX cat":
  --fileContents <- mapM readFile args
  --mapM_ putStr fileContents

wcCount :: String -> [Int]
wcCount str = [length (lines str), length (words str), length str]


--getLine :: IO String
--putStrLn :: String -> IO ()
--return :: a -> IO a
--(>>=) :: IO a -> (a -> IO b) -> IO b
