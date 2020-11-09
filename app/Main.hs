import Annealing
import Salesman
import System.IO
import System.Random

instance Solution (Path) where
    neighbour = stopSwap
    energy = totalDistance

-- Sample

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

sampleAnnealing :: IO (Annealing StdGen Path)
sampleAnnealing = do
    r <- getStdGen
    return (Annealing 0 sampleSchedule expAcceptance r samplePath)

-- Main

main :: IO ()
main = do
    initState <- sampleAnnealing
    bestSolution <- loop sampleIterations 1000 samplePath initState
    putStrLn . show $ bestSolution
    return ()

loop :: (RandomGen r, Solution s) => Step -> Step -> s -> Annealing r s -> IO s
loop maxStep reportFreq bestSolution state@(Annealing currentStep _ _ _ currentSolution) = do
        if currentStep `mod` reportFreq == 0
            then putStrLn (show currentStep ++ ": " ++ show (energy currentSolution))
            else return ()
        let nextState = anneal state
        let nextBestSolution = if energy bestSolution <= energy currentSolution
            then bestSolution
            else currentSolution
        nextBestSolution `seq` if currentStep >= maxStep 
            then return nextBestSolution
            else loop maxStep reportFreq nextBestSolution nextState
