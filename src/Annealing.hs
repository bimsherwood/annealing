module Annealing (
    Step,
    Temp,
    Schedule,
    Energy,
    Probability,
    Acceptance,
    MeasureEnergy,
    PickNeighbour,
    Annealing,
    newAnnealing,
    solution,
    step,
    energy,
    anneal,
    expAcceptance,
    randomSwap
) where

import System.Random

type Step = Int
type Temp = Double
type Schedule = Step -> Temp
type Energy = Double
type Probability = Double
type Acceptance = Temp -> Energy -> Energy -> Probability
type MeasureEnergy s = s -> Energy
type PickNeighbour s r = s -> r -> (s, r)
data Annealing s r = Annealing Step Schedule (PickNeighbour s r) (MeasureEnergy s) Acceptance s

newAnnealing :: RandomGen r
    => Schedule
    -> (PickNeighbour s r)
    -> (MeasureEnergy s)
    -> Acceptance
    -> s
    -> Annealing s r
newAnnealing = Annealing 0

solution :: Annealing s r -> s
solution (Annealing _ _ _ _ _ sol) = sol

step :: Annealing s r -> Step
step (Annealing step _ _ _ _ _) = step

energy :: Annealing s r -> Energy
energy (Annealing _ _ _ energy _ sol) = energy sol

-- Run one step of the simulated annealing algorithm
anneal :: RandomGen r => Annealing s r -> r -> (Annealing s r, r)
anneal (Annealing step schedule neighbour energy acceptance sol) rand0 =
    let temp = schedule step -- Get the current temperature
        (newSol, rand1) = neighbour sol rand0 -- Pick a neighbour
        oldEnergy = energy sol -- Measure the solution's Energy
        newEnergy = energy newSol
        acceptProb = acceptance temp oldEnergy newEnergy -- Check if the neighbour is accepted
        (randSample, rand2) = random rand1
        nextSol = if randSample <= acceptProb
            then newSol
            else sol
    in (Annealing (succ step) schedule neighbour energy acceptance nextSol, rand2)

-- A basic exponential acceptance function.
-- Better solutions are always accepted.
-- Worse solutions are accepted based on the ratio of the
-- energies, getting less likely as temperature falls.
expAcceptance :: Acceptance
expAcceptance t e eNext =
    let ratio = e / eNext
        in if ratio >= 1
            then 1
            else exp $ (ratio-1) / t

pivot :: [a] -> Int -> ([a], a, [a])
pivot lst i = (take i lst, lst!!i, drop (i+1) lst)

randomSwap :: RandomGen r => [a] -> r -> ([a], r)
randomSwap xs rand0 =
    let len = length xs
        (aIndex, rand1) = randomR (0, len - 2) rand0
        (bIndex, rand2) = randomR (0, len - aIndex - 2) rand1
        (left, a, middleRight) = pivot xs aIndex
        (middle, b, right) = pivot middleRight bIndex
    in (left ++ [b] ++ middle ++ [a] ++ right, rand2)
