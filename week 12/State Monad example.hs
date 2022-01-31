-- Taken from https://wiki.haskell.org/State_Monad

module StateGame where

import Control.Monad.State

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g 
-- 'a'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

type GameValue = Int
type GameState = (Bool, Int)

-- The State Monad is a global state of the program which can be accessed 
-- and changed
-- Similar to the map of variables to values in the evaluator
-- State x y means that 
--      x is the type of the state 
--      y is the type of the end result
-- State GameState Gamevalue means that
--      GameState ( (Bool,Int) ) is the type of the state
--      Gamevalue (Int) is the type of the end result

-- x <- get
-- get "gets" the state from the State Monad, in this case a GameState

-- put x
-- put "sets" the state of the State Monad, in this case a GameState

playGame :: String -> State GameState GameValue
playGame []     = do
    (_, score) <- get
    return score

playGame (x:xs) = do
    (on, score) <- get
    case x of
         'a' | on -> put (on, score + 1)
         'b' | on -> put (on, score - 1)
         'c'      -> put (not on, score)
         _        -> put (on, score)
    playGame xs

startState = (False, 0)

main = print $ evalState (playGame "cabb") startState