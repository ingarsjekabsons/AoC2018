{-# OPTIONS_GHC -Wall #-}
module Calibration where

import Data.List.Split

dropAllSpaces :: String -> String
dropAllSpaces s = drop' $ reverse $ drop' $ reverse s
    where drop' s' = dropWhile (\c -> c == ' ') s'

parseInput :: String -> [String]
parseInput inp = map dropAllSpaces $ splitOn "," inp

-- "-" is not a monoid, therefore operand order matters,
-- therefore flipping regular "-"
signToFun :: String -> (Int -> Int -> Int)
signToFun c = case c of
    "+" -> (+)
    "-" -> flip (-)

getNum :: String -> Int
getNum s = (read $ drop 1 s) :: Int

sToInstr :: String -> ((Int -> Int -> Int), Int)
sToInstr s = (f, i)
    where f = signToFun $ take 1 s
          i = getNum s

getListOfInstrs :: [String] -> [((Int -> Int -> Int), Int)]
getListOfInstrs = map sToInstr

partiallyApplyFuns :: [((Int -> Int -> Int), Int)] -> [(Int -> Int)]
partiallyApplyFuns ((f,i):fs) = (f i):(partiallyApplyFuns fs)
partiallyApplyFuns []         = []

chainFuns :: [(Int -> Int)] -> Int -> Int
chainFuns (f:fs) i = chainFuns fs (f i)
chainFuns []     i = i

chainFuns2 :: [(Int -> Int)] -> Int -> [Int] -> Int
chainFuns2 (f:fs) curVal allVals = case (curVal `elem` allVals) of
                                     True -> curVal
                                     False -> chainFuns2 fs (f curVal) (curVal:allVals)

getResult :: String -> Int
getResult s = chainFuns (partiallyApplyFuns $ getListOfInstrs $ parseInput s) 0

getResult2 :: String -> Int
getResult2 s = chainFuns2 (cycle $ partiallyApplyFuns $ getListOfInstrs $ parseInput s) 0 []
main :: IO ()
main = do
    input <- readFile "data.txt"
    putStrLn ("Result is: " ++ show (getResult input))
    putStrLn ("Result p2 is: " ++ show (getResult2 input))
