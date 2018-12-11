module Main where

import Coords

main :: IO ()
main = do
    input <- readFile "data.txt"
    let res = getMaxArea (mkAllCoords (lines input))
    putStrLn("Result: " ++ (show res))
