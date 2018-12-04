module Guards where

import Data.Time
import Data.List

data Action = Begins | FallsAsleep | WakesUp

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
    minutes :: Int
}


getUTC :: String -> UTCTime
getUTC s = (parseTimeOrError True defaultTimeLocale "%Y-%m-%d %R" (extractDateTime s)) :: UTCTime
    where 
        extractDateTime s = takeWhile (\x -> x /= ']') (drop 1 s)

getGuardNo :: String -> Int
getGuardNo s = read (takeWhile (\x -> x /= ' ') $ drop 1 $ dropWhile (\x -> x /= '#') s) :: Int

parseAction :: String -> Action
parseAction s = case (take 1 $ drop 1 $ dropWhile (\x -> x /= ']') s ) of
                    "G" -> Begins
                    "f" -> FallsAsleep
                    "w" -> WakesUp

preParse :: String -> TimedRecord
preParse s = TimedRecord (getUTC s) (drop 1 $ dropWhile (\x -> x /= ']') s)

xx :: [TimedRecord] -> [SleepingRecord]
xx (r:rs) = helper (r:rs) SleepingRecord  (formatTime defaultTimeLocale "%m%d" (dateTime r)) (getGuardNo $ record r) 0
    where helper (x:xs) curRec  = undefined



