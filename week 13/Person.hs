module Person where

--Daan Eijkman
--Bart Veldman

import Control.Applicative
import Parser

data Person = Person { studentName::String, studentAge::Integer, favouriteCourse::String }
  deriving (Show)

students :: [Person]
students = [elena,peter,pol,sjaak,frits,twan,marc]

elena, peter, pol, sjaak, frits, twan, marc :: Person
elena = Person "Elena" 33 "Functional Programming"
peter = Person "Peter" 57 "Imperative Programming"
pol   = Person "Pol"   36 "Object Oriented Programming"
sjaak = Person "Sjaak" 26 "Software Verification"
frits = Person "Frits" 61 "Functional Programming"
twan  = Person "Twan"  21 "Category Theory"
marc  = Person "Marc"  41 "Mathematics"

pretty :: Person -> String
pretty p = studentName p ++ " (" ++ show (studentAge p) ++ "), likes " ++ favouriteCourse p

{- Grammar:
 -
 - person = name "(" natural ")" likes course
 - course = name
 - likes  = "," "likes"
 -        | ":"
 -
 - name     = namepart | namepart name
 - namepart = letter { lower } [space]
 -}

name :: Parser String
name = unwords <$> some namepart
  where namepart = (:) <$> letter <*> many lower <* space

{- person :: Parser Person
person = do
  pname <- name <* space
  page <- symbol "(" *> nat <* symbol ")"
  symbol ","
  space 
  name 
  space 
  pcourse <- name
  return $ Person pname page pcourse -}

test :: Person -> Person
test p = case parseUnparse p of
           Just (p', "") -> p'
           Just _        -> error "parser didn't consume all input"
           Nothing       -> error "parser failed"
  where parseUnparse = parse person . pretty

