module SalesmanAnnealing (
    sampleIterations,
    sampleAnnealing
) where

import Annealing
import Salesman
import System.IO
import System.Random

samplePath :: Path
samplePath = Path [
    City 0 0,
    City 1 12,
    City 2 9,
    City 2 11,
    City 3 3,
    City 3 4,
    City 4 15,
    City 5 0,
    City 5 2,
    City 5 8,
    City 7 6,
    City 8 3,
    City 8 6,
    City 9 9,
    City 11 0,
    City 11 1,
    City 12 4,
    City 19 3]

sampleIterations :: Step
sampleIterations = 2000000

sampleSchedule :: Schedule
sampleSchedule = (4000/) . fromIntegral

sampleAnnealing :: RandomGen r => Annealing Path r
sampleAnnealing = newAnnealing sampleSchedule stopSwap totalDistance expAcceptance samplePath
