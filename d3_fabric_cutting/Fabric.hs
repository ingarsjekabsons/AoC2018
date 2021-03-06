module Fabric where

data Claim = Claim {
    claimId :: Int,
    fromLeft :: Int,
    fromTop :: Int,
    width :: Int,
    height :: Int
} deriving (Show)

dropUntilFirst :: Char -> String -> String
dropUntilFirst c s = drop 1 $ dropWhile (\x -> x /= c) s

-- example: #1 @ 1,3: 4x4
parseClaim :: String -> Claim
parseClaim s = Claim cId fLeft fTop w h
    where   cId     = read (takeWhile (\x -> x /= '@') $ dropUntilFirst '#' s ) :: Int
            fLeft   = read (takeWhile (\x -> x /= ',') $ dropUntilFirst '@' s ) :: Int
            fTop    = read (takeWhile (\x -> x /= ':') $ dropUntilFirst ',' s ) :: Int
            w       = read (takeWhile (\x -> x /= 'x') $ dropUntilFirst ':' s ) :: Int
            h       = read (dropUntilFirst 'x' s) :: Int


greatest :: (Int, Int) -> Claim -> (Int, Int)
greatest c n = (maxX, maxY)
    where maxX = case (fromLeft n + width n > fst c) of
                    True    -> (fromLeft n + width n)
                    False   -> fst c
          maxY = case (fromTop n + height n > snd c) of
                    True    -> (fromTop n + height n)
                    False   -> snd c

getMaxSize :: [Claim] -> (Int, Int)
getMaxSize = foldl greatest (0, 0)


coversInch :: Claim -> (Int,Int) -> Bool
coversInch c (x,y) = case ((x >= x1 && x <= x2) && (y >= y1 && y <= y2)) of
                        True    -> True
                        False   -> False
                       where
                            x1 = fromLeft c + 1
                            x2 = fromLeft c + width c
                            y1 = fromTop c + 1
                            y2 = fromTop c + height c


inchCoveredTimes :: [Claim] -> (Int, Int) -> Int
inchCoveredTimes (c:cs) (x, y) = case (coversInch c (x, y)) of
                                    True    -> 1 + inchCoveredTimes cs (x, y)
                                    False   -> inchCoveredTimes cs (x, y)
inchCoveredTimes [] _          = 0

calcOverlaps :: [Claim] -> [((Int, Int), Int)]
calcOverlaps cs = fmap (\coords ->
                            (coords, inchCoveredTimes cs coords))
                                        [(x, y) | x <- [1..(fst $ getMaxSize cs)],
                                                  y <- [1..(snd $ getMaxSize cs)]]

getRes1 :: [String] -> Int
getRes1 ss = length $ filter (\x -> snd x >= 2) $ calcOverlaps $ fmap parseClaim ss

getArea :: Claim -> [(Int, Int)]
getArea c = [(x, y) | x <- [(fromLeft c + 1)..(fromLeft c + width c)],
                      y <- [(fromTop c + 1)..(fromTop c + height c)]]

extractCoverage :: [((Int, Int), Int)] -> (Int, Int) -> Int
extractCoverage (a:as) (x,y) = case (fst (fst a) == x && snd (fst a) == y) of
                                    True -> snd a
                                    False -> extractCoverage as (x,y)

isOnlyInhabitant :: Claim -> [((Int, Int), Int)] -> Bool
isOnlyInhabitant c as = inspect (getArea c)
    where inspect (x:xs) = case (extractCoverage as x) of
                            1 -> inspect xs
                            _ -> False
          inspect []     = True

getRes2 :: [String] -> [Claim]
getRes2 ss = filter (\x -> isOnlyInhabitant x overlaps) claims
        where claims = fmap parseClaim ss
              overlaps = calcOverlaps claims

main :: IO ()
main = do
    inp <- readFile "data.txt"
    putStrLn ("Result 1: " ++ show (getRes1 $ lines inp))
    putStrLn ("Result2 is : " ++ show (getRes2 $ lines inp))
