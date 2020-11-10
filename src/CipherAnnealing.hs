module CipherAnnealing (
    sampleIterations,
    sampleAnnealing
) where

import Annealing
import Cipher
import English
import System.IO
import System.Random

sampleKey :: Key
sampleKey = read ['a'..'z']

sampleIterations :: Step
sampleIterations = 30000

sampleSchedule :: Schedule
sampleSchedule = (70/) . fromIntegral

sampleAnnealing :: RandomGen r => QuadgramTable -> String -> Annealing Key r
sampleAnnealing qTable ctxt =
    let energy k = negate . fitness qTable . decipher k $ ctxt
    in newAnnealing sampleSchedule letterSwap energy expAcceptance sampleKey