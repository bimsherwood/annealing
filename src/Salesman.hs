module Salesman (
    City(..),
    Path(..),
    distance,
    totalDistance,
    stopSwap
) where

import System.Random

data City = City Double Double
    deriving (Show)

data Path = Path [City]
    deriving (Show)

distance :: City -> City -> Double
distance (City x1 y1) (City x2 y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2

totalDistance :: Path -> Double
totalDistance (Path []) = 0
totalDistance (Path [_]) = 0
totalDistance (Path (x:y:xs)) = (distance x y) + totalDistance (Path (y:xs))

pivot :: [a] -> Int -> ([a], a, [a])
pivot lst i = (take i lst, lst!!i, drop (i+1) lst)

stopSwap :: RandomGen r => Path -> r -> (Path, r)
stopSwap (Path path) rand0 =
    let len = length path
        (aIndex, rand1) = randomR (0, len - 2) rand0
        (bIndex, rand2) = randomR (0, len - aIndex - 2) rand1
        (left, a, middleRight) = pivot path aIndex
        (middle, b, right) = pivot middleRight bIndex
    in (Path $ left ++ [b] ++ middle ++ [a] ++ right, rand2)