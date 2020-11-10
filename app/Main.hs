import Annealing
import SalesmanAnnealing
import English
import Cipher
import System.Environment
import System.IO
import System.Random

import Prelude hiding (lookup)
import Data.HashMap hiding (map)

main :: IO ()
main = sample

sample :: IO ()
sample = do
    rand <- getStdGen
    bestSolution <- annealingLoop sampleIterations 1000 sampleAnnealing sampleAnnealing rand
    putStrLn . show $ bestSolution

annealingLoop :: RandomGen r => Step -> Step -> Annealing s r -> Annealing s r -> r -> IO s
annealingLoop maxStep reportFreq bestState state rand = do
    if step state `mod` reportFreq == 0
        then putStrLn (show (step state) ++ ": " ++ show (energy state))
        else return ()
    let (nextState, nextRand) = anneal state rand
    let nextBestSolution = if energy bestState <= energy state
        then bestState
        else state
    nextBestSolution `seq` if step state >= maxStep 
        then return . solution $ nextBestSolution
        else annealingLoop maxStep reportFreq nextBestSolution nextState nextRand
