-- Simple functions

showResult :: Int -> String

showResult n =
  "The result is " ++ show n

showAreaOfCircle :: Float -> String

showAreaOfCircle n =
  "The area of a circle with radius " ++ show n ++ "cm is about " ++ show (pi * n^2) ++ "cm^2"

-- Conditional functions

maxImp :: Ord a => a -> a -> a

maxImp x y = if x >= y then x else y

signumImp :: (Ord a, Num a) => a -> Int

signumImp x | x <  0  = -1
         | x == 0  = 0
         | x >  0  = 1


-- Binders

myNumber :: Floating a => a

myNumber = 17

-- lists

firstTenPrimes :: [Int]
firstTenPrimes  = [2, 3, 5, 7, 11, 13, 17, 19, 23, 27]

oneToTwenty :: [Int]
oneToTwenty = [1..20]

-- tuples

addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)

-- get stuff from lists

head :: [a] -> a
head (x:xs) = x

-- exercises

isLower :: Char -> Bool
isLower c = if c < 'Z' then False else True

mangle :: String -> String
-- removes first letter of word and attaches at end
mangle [] = []
mangle (x:xs)   | length xs == 0 = [x]
                | otherwise = xs ++ [x]

divide :: Int -> Int -> Int
-- divide 5 10 should be 2 while divide 5 8 should be 1
divide a b  | b - a < 0 = 0
            | b - a >= 0 = 1 + divide 2 b-a


