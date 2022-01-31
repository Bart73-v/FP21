module Trivia where

--Daan Eijkman
--Bart Veldman

import Control.Applicative
import Parser

dot :: Parser Char
dot = char '.'

{- loop :: Parser Char
loop = many (many char)
 -}
dots :: Parser (String, String)
dots = (\x y -> (x,y)) <$> many dot <*> many dot
