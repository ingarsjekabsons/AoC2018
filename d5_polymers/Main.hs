module Main where

import qualified Data.Vector.Unboxed as V
import Reduce

main :: IO ()
main = do
    input <- readFile "data.txt"
    let res = reduce (mkPolymer ( (lines input) !! 0))
    let res2 = getShortest (mkPolymer ( (lines input) !! 0)) ['a'..'z']
    putStrLn("Result is: " ++ show (V.length res))
    putStrLn("Result2 is: " ++ show (res2))

