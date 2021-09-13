--Daan Eijkman
--Bart Veldman

module Database where

type Person = (Name, Age, FavouriteCourse)

type Name             = String
type Age              = Integer
type FavouriteCourse  = String

elena, peter, pol, bart :: Person
elena  =  ("Elena",  33,  "Functional Programming")
peter  =  ("Peter",  57,  "Imperative Programming")
pol    =  ("Pol",    36,  "Object Oriented Programming")
bart   =  ("Bart",   27,  "Functional Programming")

students :: [Person]
students = [elena, peter, pol, bart]

age :: Person -> Age
age (_, n, _)  =  n

name :: Person -> Name
name (n, _, _) = n

favouriteCourse :: Person -> FavouriteCourse
favouriteCourse (_, _, n) = n

showPerson       :: Person -> String
showPerson (n, a, f) = n ++ ". Age: " ++ show a ++ ". Favourite course: " ++ f 

twins :: Person -> Person -> Bool
twins (_, a, _) (_, b, _) = a == b 

increaseAge :: Person -> Person
increaseAge (n, a, f) = (n, a+1, f)

incrementTwice = map (increaseAge . increaseAge) students

promote = let pr (n, a, f) = ("dr. " ++ n, a, f) in map pr students

findFrits = let frits p = name p == "Frits" in filter frits students

findTwenties = let twenties p = age p >= 20 && age p < 30 in filter twenties students

averageAge = sum (map (toInteger . age) students) `div` toInteger (length students)

promoteFP = let pr (n, a, f) = if f == "Functional Programming" then ("dr. " ++ n, a, f) else (n, a, f)
            in map pr students
