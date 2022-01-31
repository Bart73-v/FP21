{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

import Data.Char

import Control.Monad
import Control.Applicative

type State st a = st -> (a, st)

newtype Parser a = P { parse :: String -> Maybe (a, String) }
  deriving (Functor)

instance Applicative Parser where
  pure  = return
  (<*>) = ap


instance Monad Parser where
  return :: a -> Parser a -- = a -> String -> Maybe (a, String)
  return x = P (\inp -> Just (x, inp))
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  m >>= f = P (\inp -> case parse m inp of
                           Nothing -> Nothing
                           Just (r, rinp) -> parse (f r) rinp)                        

instance Alternative Parser where
  empty :: Parser a
  empty = P (\inp -> Nothing)
  
  (<|>) :: Parser a -> Parser a -> Parser a
  pl <|> pr = P $ \inp -> case parse pl inp of
                           Nothing -> parse pr inp
                           success -> success                           
  
item :: Parser Char
item = P $ \inp ->
  case inp of
    []     -> Nothing
    (x:xs) -> Just (x, xs)

--


sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c  then return c else empty

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string = mapM char

-- do c <- char x
--                   cs <- string xs
--                   return (c:cs)

-- pure (:) <*> char x <*> string xs 

--

ident :: Parser String
ident = (:) <$> sat isLower <*> many (sat isAlphaNum)

nat :: Parser Integer
nat = read <$> some (sat isDigit) 

int :: Parser Integer
int =  (* (-1)) <$> (char '-' *> nat) <|> nat

space :: Parser ()
space = many (sat isSpace) *> return ()

--

symbol :: String -> Parser String
symbol symb = string symb <* space

identifier :: Parser String
identifier = ident <* space

integer :: Parser Integer
integer = int <* space

--
   
{-
  a parser for parsing function calls
      f(1,2,3)
  
  funCall = ident '(' argList ')'
  
  argList = arg { ',' arg }
          | ""
          
  arg = int
  
-}

funCall :: Parser (String, [Integer])
funCall = (,) <$> identifier <*> (symbol "(" *> argList <* symbol ")")

argList :: Parser [Integer]
argList = do a <- arg
             as <- many ( symbol "," *> arg )
             return (a:as)
          <|> return []

arg = integer


atMost :: Int -> Parser a -> Parser [a]
atMost = undefined

between :: (Int,Int) -> Parser a -> Parser [a]
between = undefined

anythingBut :: Parser a -> Parser ()
anythingBut = undefined