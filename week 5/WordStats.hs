import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as M
--Daan Eijkman
--Bart Veldman

wordFrequency :: String -> [(String,Int)]
wordFrequency  = map (\x->(head x,length x)) . group . sort . words

mostFrequentOfLength :: Int -> String -> [String]
mostFrequentOfLength n = map fst . sortOn snd . filter (\(s,x) -> length s > n) . wordFrequency

wordLengthFrequency :: String -> [(Int, Int)]
wordLengthFrequency = map (\x -> (head x, length x)) . group . sort . map length . words

--anagrams :: String -> ??

{- this 'main' function is only here for the compiled, stand-alone version 
 - calling it from within GHCi is problematic since GHCi itself needs stdin!
 - to compile, run:
 -
 -     ghc -O WordStats 
 -
 - (The -O flag turns on optimizations)
 -}    
main :: IO ()
main = onStdin $ mostFrequentOfLength 5  -- change this to run a different function from the commandline
  where onStdin f = getContents >>= (mapM_ print . f . filter (\x->isAlphaNum x || isSpace x))
