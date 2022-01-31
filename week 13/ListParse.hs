{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module ListParse where

--Daan Eijkman
--Bart Veldman

import Control.Applicative
import Control.Monad
import Parser

{- grammar:
 -   intList   = "{" { integer } "}"
 -}

intList :: Parser [Integer]
intList = (:) <$> symbol "{" *>  many (space *> int) <* symbol "}"


{- grammar:
 -   intRecord = "{" integer "#" { integer } "}"
 -                   ^ =: n      ^^^^^^^^^^^ (repeat n# times)
 -}

intRecord :: Parser [Integer]
intRecord = do {symbol "{"; n <- int; symbol "#"; output <- atMost (fromInteger n) integer; symbol "}"; return output}

