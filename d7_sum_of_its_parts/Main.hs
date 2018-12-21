module Main where

import Deps

main :: IO ()
main = do
    input <- readFile "data.txt"
    let depMap = makeDepMap $ parseDeps $ lines input
    putStrLn ("Result is : " ++ reverse (solve depMap))
