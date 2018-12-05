module Guards where

import Data.Time
import Data.List

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
                                                (fromTo curRec)
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



