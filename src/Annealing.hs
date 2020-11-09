module Annealing (
    Step,
    Temp,
    Schedule,
    Energy,
    Probability,
    Acceptance,
    Annealing(..),
    Solution,
    neighbour,
    energy,
    anneal,
    expAcceptance
) where

import System.Random

type Step = Int
type Temp = Double
type Schedule = Step -> Temp
type Energy = Double
type Probability = Double
type Acceptance = Temp -> Energy -> Energy -> Probability
data Annealing rand sol = Annealing Step Schedule Acceptance rand sol

class Solution a where
    neighbour :: RandomGen r => a -> r -> (a, r)
    energy :: a -> Energy

-- Run one step of the simulated annealing algorithm
anneal :: (RandomGen r, Solution s) => Annealing r s -> Annealing r s
anneal (Annealing step schedule acceptance rand0 sol) =
    let temp = schedule step -- Get the current temperature
        (newSol, rand1) = neighbour sol rand0 -- Pick a neighbour
        oldEnergy = energy sol -- Measure the solution's Energy
        newEnergy = energy newSol
        acceptProb = acceptance temp oldEnergy newEnergy -- Check if the neighbour is accepted
        (randSample, rand2) = random rand1
        nextSol = if randSample <= acceptProb
            then newSol
            else sol
    in Annealing (succ step) schedule acceptance rand2 nextSol

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
