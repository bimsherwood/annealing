module English (
    LogProb,
    Quadgram,
    QuadgramTable,
    loadQuadgramFreqs,
    logProb,
    fitness
) where

import Data.Char
import Data.Hashable
import Data.HashMap hiding (map)
import System.IO

type Frequency = Int
type LogProb = Double
data Quadgram = Quad Char Char Char Char
    deriving Eq
data QuadgramTable = QuadTable LogProb (Map Quadgram LogProb)

instance Show (Quadgram) where
    show (Quad a b c d) = [a,b,c,d]

instance Ord (Quadgram) where
    compare a b = compare (show a) (show b)

instance Hashable (Quadgram) where
    hashWithSalt s = hashWithSalt s . show

instance Read (Quadgram) where
    readsPrec _ (a:b:c:d:xs) = [(Quad (toUpper a) (toUpper b) (toUpper c) (toUpper d),xs)]
    readsPrec _ _ = undefined

parseQuadgramLine :: String -> (Quadgram, Frequency)
parseQuadgramLine line =
    let quadStr = take 4 line
        freqStr = drop 5 line
    in (
        Quad (quadStr!!0) (quadStr!!1) (quadStr!!2) (quadStr!!3),
        read freqStr
    )

readQuadgramLine :: Handle -> IO (Maybe String)
readQuadgramLine h = do
    eof <- hIsEOF h
    if eof
        then return Nothing
        else do
            line <- hGetLine h
            return $ if line == "" then Nothing else Just line

parseQuadgramFreqs :: Handle -> IO [(Quadgram, Frequency)]
parseQuadgramFreqs h = do
    eof <- hIsEOF h
    line <- readQuadgramLine h
    case line of
        Nothing -> return []
        Just str -> do
            nextFreqs <- parseQuadgramFreqs h
            let quadFreq = parseQuadgramLine str
            return (quadFreq:nextFreqs)

freqToLogProb :: Frequency -> Frequency -> LogProb
freqToLogProb total freq
    | freq == 0 = -log(fromIntegral total)
    | otherwise = -log(fromIntegral total / fromIntegral freq)

loadQuadgramFreqs :: Handle -> IO QuadgramTable
loadQuadgramFreqs h = do
    freqList <- parseQuadgramFreqs h
    let total = sum . map snd $ freqList
    let minLogProb = freqToLogProb total 1
    return . QuadTable minLogProb . fromList . map (\(q, f) -> (q, freqToLogProb total f)) $ freqList

logProb :: QuadgramTable -> Quadgram -> LogProb
logProb (QuadTable min map) k = maybe min id (Data.HashMap.lookup k map)

fitness :: QuadgramTable -> String -> LogProb
fitness = fitness' 0 0

fitness' :: Int -> LogProb -> QuadgramTable -> String -> LogProb
fitness' quads acc qTable str
    | length (take 4 str) < 4 = acc / (fromIntegral quads)
    | otherwise =
        let quad = read . take 4 $ str
            quadLogProb = logProb qTable quad
        in fitness' (quads + 1) (acc + quadLogProb) qTable (tail str)
