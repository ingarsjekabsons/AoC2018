module Inventory where

timesElem :: String -> Char -> Int
timesElem s c = count s c 0
    where count (s:ss) c a = case (s == c) of
                                True  -> count ss c (a+1)
                                False -> count ss c a
          count [] c a = a

dropElem :: String -> Char -> String
dropElem s c = filter (\x -> x /= c) s

countElems :: String -> [(Int, Char)]
countElems a@(s:ss) = ((timesElem a s), s):(countElems (dropElem ss s))
countElems []       = []


countSignificants :: [(Int, Char)] -> (Int, Int) -> (Int, Int)
countSignificants a b = ((fst b) + x, (snd b) + y)
    where x = case (filter (\i -> fst i == 2) a) of
                (_:_) -> 1
                []    -> 0
          y = case (filter (\i -> fst i == 3) a) of
                (_:_) -> 1
                []    -> 0

count :: (Int, Int) -> String -> (Int, Int)
count counts s = countSignificants (countElems s) counts


main :: IO ()
main = do
    input <- readFile "data.txt"
    let r = foldl count (0,0) (lines input) 
    putStrLn ("Result is: " ++ show ((fst r) * (snd r)))
