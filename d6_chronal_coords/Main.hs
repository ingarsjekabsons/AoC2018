module Main where

import Coords

main :: IO ()
main = do
    input <- readFile "data.txt"
    let res = getMaxArea (mkAllCoords (lines input))
    let res2 = getRes2 (mkAllCoords (lines input))
    putStrLn("Result: " ++ (show res))
    putStrLn("Result2: " ++ show res2)
