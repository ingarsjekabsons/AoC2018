module Guards where

import Data.Time
import Data.List
import qualified Data.Map as M

data Action = Begins | FallsAsleep | WakesUp
    deriving Show

-- raw records:
-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up

data TimedRecord = TimedRecord {
    dateTime :: UTCTime,
    record   :: String
} deriving (Show)



-- make Eq and Ord in order to sort events in chronological order
instance Eq TimedRecord where
    (TimedRecord t1 _) == (TimedRecord t2 _) = t1 == t2

instance Ord TimedRecord where
    TimedRecord t1 _ `compare` TimedRecord t2 _ = t1 `compare` t2


data SleepingRecord = SleepingRecord {
    mmdd    :: String,
    guard   :: Int,
    minutes :: Int,
    fromTo  :: [(Int, Int)]
} deriving Show

sumByGuard :: [SleepingRecord] -> M.Map Int Int -> M.Map Int Int
sumByGuard (r:rs) curMap = case (M.member (guard r) curMap) of
                               True     -> sumByGuard rs (M.adjust ((minutes r) +) (guard r) curMap)
                               False    -> sumByGuard rs (M.insert (guard r) (minutes r) curMap)
sumByGuard [] curMap     = curMap

uniqGuards :: [SleepingRecord] -> [Int]
uniqGuards recs = doIt recs []
    where doIt (r:rs) acc = case (elem (guard r) acc) of
                                True    -> doIt rs acc
                                False   -> doIt rs ((guard r):acc)
          doIt [] acc     = acc

hottestPerGuard :: [SleepingRecord] -> Int -> (Int,Int)
hottestPerGuard recs g = getHottestMinute $ sleepyMinuteHeatMap (filter (\x -> guard x == g) recs) M.empty

getAllRecords :: [SleepingRecord] -> [Int] -> [(Int, (Int,Int))]
getAllRecords recs gs = fmap (\g -> (g, (hottestPerGuard recs g))) gs

sleepyMinuteHeatMap :: [SleepingRecord] -> M.Map Int Int -> M.Map Int Int
sleepyMinuteHeatMap recs curMap = foldl recordToHeat curMap recs

recordToHeat :: M.Map Int Int -> SleepingRecord -> M.Map Int Int
recordToHeat m r = foldl (\m r -> if (M.member r m) then (M.adjust (1 +) r m) else (M.insert r 1 m)) m $ allMins r
    where allMins r = concat $ fmap (\x -> [fst x .. (snd x - 1)]) (fromTo r)

getGreatestSleeper :: M.Map Int Int -> Int
getGreatestSleeper = fst . M.foldrWithKey (\k v c -> if (v > snd c) then (k,v) else c) (0,0)

getHottestMinute :: M.Map Int Int -> (Int, Int)
getHottestMinute = M.foldrWithKey (\k v c -> if (v > snd c) then (k,v) else c) (0,0)

getRes1 :: String -> (Int, (Int, Int))
getRes1 s = (sleeper, hottestMinStats)
    where
        sleeper = getGreatestSleeper sleepingStats
        sleepingStats = sumByGuard sleepingRecords M.empty
        sleepingRecords = mkSleepRecords (sort $ fmap preParse $ lines s)
        hottestMinStats = getHottestMinute (sleepyMinuteHeatMap (filter (\x -> guard x == sleeper) sleepingRecords) M.empty)

getRes2 :: String -> (Int, (Int,Int))
getRes2 s = maxHeat
    where maxHeat = foldl (\r acc -> if ((snd (snd r)) > snd (snd acc)) then r else acc) (0, (0,0)) records
          records = getAllRecords sRecs guards
          sRecs   = mkSleepRecords (sort $ fmap preParse $ lines s)
          guards  = uniqGuards sRecs


makeMMDD :: UTCTime -> String
makeMMDD t = formatTime defaultTimeLocale "%m%d" t

getMins :: UTCTime -> Int
getMins t = read (formatTime defaultTimeLocale "%M" t) :: Int

getUTC :: String -> UTCTime
getUTC s = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d %R" (extractDateTime s)) :: UTCTime
    where
        extractDateTime s = takeWhile (\x -> x /= ']') (drop 1 s)

getGuardNo :: String -> Int
getGuardNo s = read (takeWhile (\x -> x /= ' ') $ drop 1 $ dropWhile (\x -> x /= '#') s) :: Int

parseAction :: String -> Action
parseAction s = case (take 1 $ drop 1 s ) of
                    "G" -> Begins
                    "f" -> FallsAsleep
                    "w" -> WakesUp

preParse :: String -> TimedRecord
preParse s = TimedRecord (getUTC s) (drop 1 $ dropWhile (\x -> x /= ']') s)

-- we assume first record has action Begins
mkSleepRecords :: [TimedRecord] -> [SleepingRecord]
mkSleepRecords (r:rs) = helper rs (SleepingRecord  (makeMMDD (dateTime r)) (getGuardNo $ record r) 0 []) 0 (0,0) Begins
    where helper (x:xs) curRec mins p action =
            case (parseAction $ record x) of
                Begins      -> curRec : helper xs
                                           (SleepingRecord
                                                (makeMMDD (dateTime x))
                                                (getGuardNo $ record x)
                                                0
                                                []
                                           ) 0 (0,0) Begins
                FallsAsleep -> helper xs
                                    (SleepingRecord
                                            (makeMMDD $ dateTime x)
                                            (guard curRec)
                                            (minutes curRec)
                                            (fromTo curRec)
                                    ) (getMins (dateTime x)) ((getMins (dateTime x)),0) FallsAsleep
                WakesUp     -> helper xs (SleepingRecord
                                                (makeMMDD $ dateTime x)
                                                (guard curRec)
                                                (getMins (dateTime x) - mins + (minutes  curRec))
                                                ((fst p, (getMins $ dateTime x)):(fromTo curRec))
                                         ) 0 (0,0) WakesUp
          helper [] curRec _ _ _ = curRec:[]



