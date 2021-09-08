module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")

students :: [Person]
students = [elena, peter, pol]

age :: Person -> Age
age (_, n, _)  =  n

-- name             :: Person -> Name
-- favouriteCourse  :: Person -> FavouriteCourse
-- showPerson       :: Person -> String
-- twins            :: Person -> Person -> Bool
-- increaseAge      :: Person -> Person
