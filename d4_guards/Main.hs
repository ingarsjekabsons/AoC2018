module Main where

import Guards

main :: IO ()
main = do
    input <- readFile "data.txt"
    let res = getRes1 input
    putStrLn("Result 1 is, sleeper:  "  ++ show (fst res) 
                    ++ ", sleepiest minute: " ++ show (fst . snd $ res) 
                    ++ ", slept: " ++ show (snd . snd $ res) 
                    ++ " times. (==> " ++ show ((fst res) * (fst . snd $ res)) ++ ")")
