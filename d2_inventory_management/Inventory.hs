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

differsBy :: String -> String -> Int
differsBy (s:ss) (s':ss') = case (s == s') of
                                True  -> differsBy ss ss'
                                False -> 1 + differsBy ss ss'
differsBy [] [] = 0
differsBy _  _  = 0

produceList :: [String] -> Int -> [String]
produceList (s:ss) i = take i (repeat s) ++ produceList ss i
produceList [] _     = []

findDiffersByOne :: [String] -> [String] -> (String, String)
findDiffersByOne (s:ss) (s':ss') = case (differsBy s s') of
                                       1 -> (s, s')
                                       _ -> findDiffersByOne ss ss'
findDiffersByOne [] _            = ("Not found","Not found")

getCommonChars :: String -> String -> String
getCommonChars (s:ss) (s':ss') = case (s == s') of
                                    True  -> s : getCommonChars ss ss'
                                    False -> getCommonChars ss ss'
getCommonChars [] _            = []

main :: IO ()
main = do
    input <- readFile "data.txt"
    let r = foldl count (0,0) (lines input)
    let rr = findDiffersByOne (produceList (lines input) (length $ lines input)) (cycle $ lines input)
    putStrLn ("Result is: " ++ show ((fst r) * (snd r)))
    putStrLn ("Result part 2 is: \n " ++ show (getCommonChars (fst rr) (snd rr)))
