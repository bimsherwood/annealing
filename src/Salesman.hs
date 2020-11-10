module Salesman (
    City(..),
    Path(..),
    distance,
    totalDistance,
    stopSwap
) where

import Annealing
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

stopSwap :: RandomGen r => Path -> r -> (Path, r)
stopSwap (Path path0) r0 = let (path1, r1) = randomSwap path0 r0 in (Path path1, r1)