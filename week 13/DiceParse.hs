module ParseDice where

--Daan Eijkman
--Bart Veldman

import Control.Applicative
import Parser
import Dice

{-

expr     = fraction

fraction = formula
         | term, "/", positive

formula  = formula, "+", term
         | formula, "-", term
         | term

term     = "(", expr, ")"
         | integer                  -- constants
         | [positive], "d", positive -- dice

positive = <an integer greater than 0>

-}

expr :: Parser Expr
expr = fraction

fraction :: Parser Expr
fraction = do
      f <- formula
      Div f <$ symbol "/" <*> positive <|> return f

formula :: Parser Expr
formula = do 
      t <- term
      (:+:) t <$ symbol "+" <*> formula <|> Sub t <$ symbol "-" <*> formula <|> return t
      -- puts + and - between brackets ???

term :: Parser Expr
term = Dice <$ char 'd' <*> integer
      {- do n <- positive
      some positive <* char 'd'
      times n :+: -}
      <|> Lit <$> integer
      <|> symbol "(" *> expr <* symbol ")"
-- Does not parse multiple dice.

positive :: Parser Integer
positive = integer

-- test cases: a list of tuples listing the input and output of "parseAll expr"
-- in case you used a different constructor for division, edit the definition of "</>"
test :: [(String, Maybe Expr)]
test = [ ""          =-> Nothing
       , "2"         =-> Just $ Lit 2
       , "d6"        =-> Just $ Dice 6
       , "(d6)"      =-> Just $ Dice 6
       , "((d6))"    =-> Just $ Dice 6
       , "2d10"      =-> Just $ Dice 10 :+: Dice 10
       , "xkcd"      =-> Nothing
       , "d6+d8"     =-> Just $ Dice 6 :+: Dice 8
       , "d10-1"     =-> Just $ Sub (Dice 10) (Lit 1)
       , "1+d2+d3"   =-> Just $ Lit 1 :+: Dice 2 :+: Dice 3
       , "6-5-4"     =-> Just $ Sub (Sub (Lit 6) (Lit 5)) (Lit 4)
       , "d6/2"      =-> Just $ Div (Dice 6) 2
       , "2/d6"      =-> Nothing
       , "1+2/3"     =-> Nothing
       , "1+(2/3)"   =-> Just $ Lit 1 :+: (Div (Lit 2) 3)
       , "(1+2)/3"   =-> Just $ Div (Lit 1 :+: Lit 2) 3
       ]
  where (=->) = (,)
        infixr 0 =->
        --(</>) = (:/:)             -- data Expr = ... | Expr :/: Int | ...
        --(</>) = Div             --             ... | Div Expr Int | ...
        --x </> y = x :/: Lit y   --             ... | Expr :/: Expr | ...
        --x </> y = Div x (Lit y) --             ... | Div Expr Expr | ...

-- use this function to get all incorrect answers
deviations :: (Eq b) => (a->b) -> [(a,b)] -> [(a,b,b)]
deviations f ans = [ (x,y,f x) | (x,y) <- ans, f x /= y ]

-- combine with the functions in dice
calculate :: (Fractional a) => String -> Maybe a
calculate str = expectation <$> parseAll expr str
