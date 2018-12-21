module Deps where

import Data.List (sort, nub, delete)

type Dep = (Char, Char)

-- Step C must be finished before step A can begin.
-- (C, A) -> A depends on C

doTimes :: (a -> a) -> a -> Int -> a
doTimes f s n = case (n == 0) of
                   True     -> s
                   False    -> doTimes f (f s) (n-1)

mkDep :: String -> Dep
mkDep s = (from, to)
  where from = head $ doTimes takeChar s 1
        to   = head $ doTimes takeChar s 7
        takeChar s = drop 1 $ dropWhile (/= ' ') s

parseDeps :: [String] -> [Dep]
parseDeps = fmap mkDep

findDeps :: Char -> [Dep] -> [Char]
findDeps c (d:ds) = if snd d == c then fst d : findDeps c ds else findDeps c ds
findDeps c []     = []

makeDepMap :: [Dep] -> [(Char, [Char])]
makeDepMap ds = map (\x -> (x, findDeps x ds)) uniqSteps
    where uniqSteps = nub $ sort $ concat $ map (\x -> fst x : snd x : []) ds

sortByDeps :: [(Char, [Char])] -> [(Char, [Char])]
sortByDeps ss= fmap (\x -> (snd x, fst x)) $ sort $ fmap (\x -> (snd x, fst x)) ss

traverseAndRemoveDep :: Char -> [(Char, [Char])] -> [(Char, [Char])]
traverseAndRemoveDep c dm = fmap (\x -> if (c `elem` snd x) then (fst x, delete c (snd x)) else x) dm

solve :: [(Char, [Char])] -> [Char]
solve dm = go (sortByDeps dm) []
    where go (m:m':ms) done = case (depsDone (snd m) done) of
                                True    -> go (sortByDeps $ traverseAndRemoveDep (fst m) (m':ms)) (fst m : done)
                                False   -> error "No further steps..."
          go (m:[]) done    = (fst m):done


depsDone :: [Char] -> [Char] -> Bool
depsDone (r:rs) done = case (r `elem` done) of
                           True -> depsDone rs done
                           False -> False
depsDone [] _ = True
