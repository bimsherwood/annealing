module Cipher (
    Key,
    cipher,
    decipher,
    letterSwap
) where

import Annealing
import Data.Char
import Data.Sort
import System.Random

type Permutation = [Int]
data Key = Key Permutation

instance Show (Key) where
    show (Key k) = map intToUpper k

instance Read (Key) where
    readsPrec _ = return . (\x -> (x,[])) . Key . map charToInt

charToInt :: Char -> Int
charToInt c
    | c >= 'a' = ord c - 97
    | otherwise = ord c - 65

intToUpper :: Int -> Char
intToUpper = chr . (+65)

intToLower :: Int -> Char
intToLower = chr . (+97)

permute :: Permutation -> [a] -> [a]
permute p xs = do
    pos <- [0 .. length xs - 1]
    let replacementPos = p!!pos
    let replacement = xs!!replacementPos
    return replacement

negatePerm :: Permutation -> Permutation
negatePerm p =
    let pPos = zip p [0..]
        sorted = sortOn fst pPos
    in map snd sorted

pivot :: [a] -> Int -> ([a], a, [a])
pivot lst i = (take i lst, lst!!i, drop (i+1) lst)

letterSwap :: RandomGen r => Key -> r -> (Key, r)
letterSwap (Key k0) r0 = let (k1, r1) = randomSwap k0 r0 in (Key k1, r1)

normalise :: String -> String
normalise [] = []
normalise (x:xs)
    | x >= 'a' && x <= 'z' = (toUpper x) : normalise xs
    | x >= 'A' && x <= 'Z' = x : normalise xs
    | otherwise = normalise xs

cipher :: Key -> String -> String
cipher k = cipher' k . normalise

decipher :: Key -> String -> String
decipher (Key p) = map toLower . cipher (Key (negatePerm p))

cipher' :: Key -> String -> String
cipher' _ [] = []
cipher' k@(Key p) (x:xs) =
    let subst = intToUpper . (p!!) . charToInt $ x
    in subst : cipher' k xs

