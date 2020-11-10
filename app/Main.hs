import Annealing
import CipherAnnealing
import English
import Cipher
import System.Environment
import System.IO
import System.Random

import Prelude hiding (lookup)
import Data.HashMap hiding (map)

main :: IO ()
main = sample

sampleCText :: String
sampleCText = "VJG VJKPIU KP QWT EQPVTQN CTG DA PCVWTG HTGG, WPTGUVTCKPGF, WPJKPFGTGF; DWV VJQUG PQV KP QWT EQPVTQN CTG YGCM, UNCXKUJ, TGUVTCKPGF, DGNQPIKPI VQ QVJGTU. TGOGODGT, VJGP, VJCV KH AQW UWRRQUG VJCV VJKPIU YJKEJ CTG UNCXKUJ DA PCVWTG CTG CNUQ HTGG, CPF VJCV YJCV DGNQPIU VQ QVJGTU KU AQWT QYP, VJGP AQW YKNN DG JKPFGTGF. AQW YKNN NCOGPV, AQW YKNN DG FKUVWTDGF, CPF AQW YKNN HKPF HCWNV DQVJ YKVJ IQFU CPF OGP. DWV KH AQW UWRRQUG VJCV QPNA VQ DG AQWT QYP YJKEJ KU AQWT QYP, CPF YJCV DGNQPIU VQ QVJGTU UWEJ CU KV TGCNNA KU, VJGP PQ QPG YKNN GXGT EQORGN AQW QT TGUVTCKP AQW. HWTVJGT, AQW YKNN HKPF HCWNV YKVJ PQ QPG QT CEEWUG PQ QPG. AQW YKNN FQ PQVJKPI CICKPUV AQWT YKNN. PQ QPG YKNN JWTV AQW, AQW YKNN JCXG PQ GPGOKGU, CPF AQW PQV DG JCTOGF."

sampleQuadgramFile :: String
sampleQuadgramFile = "./english_quadgrams.txt"

sample :: IO ()
sample = do
    h <- openFile sampleQuadgramFile ReadMode
    qTable <- loadQuadgramFreqs h
    rand <- getStdGen
    let sample = sampleAnnealing qTable sampleCText
    bestSolution <- annealingLoop sampleIterations 20 sample sample rand
    putStrLn . show $ bestSolution
    putStrLn . show . decipher bestSolution $ sampleCText

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
