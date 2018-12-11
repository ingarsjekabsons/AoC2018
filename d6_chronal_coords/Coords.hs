module Coords where
import qualified Data.Vector as V
import Data.List (sort)
import Data.Maybe (fromJust)

type Coord = (Int, Int)
type Distance = Int

distBetween :: Coord -> Coord -> Distance
distBetween (x,y) (x',y') = abs (x - x') + abs (y - y')

mkCoord :: String -> Coord
mkCoord s = let x = read (takeWhile ((/=) ',') s) :: Int
                y = read (drop 1 $ dropWhile ((/=) ',') s) :: Int
            in (x, y)

mkAllCoords :: [String] -> [Coord]
mkAllCoords = fmap mkCoord

getEdges :: [Coord] -> (Coord, Coord)
getEdges cs = let minX = foldl (\c n -> if (fst n < c) then fst n else c) (maxBound :: Int) cs
                  maxX = foldl (\c n -> if (fst n > c) then fst n else c) 0 cs
                  minY = foldl (\c n -> if (snd n < c) then snd n else c) (maxBound :: Int) cs
                  maxY = foldl (\c n -> if (snd n > c) then snd n else c) 0 cs
              in ( (minX, minY), (maxX, maxY) )

testInfinity :: [Coord] -> Coord -> (Coord, Coord) -> Bool
testInfinity cs c edges = let testLeft      = fst (closestToCoord cs ((fst . fst $ edges)-1, snd c)) == c
                              testRight     = fst (closestToCoord cs ((fst . snd $ edges)+1, snd c)) == c
                              testTop       = fst (closestToCoord cs (fst c, (snd . fst $ edges)-1)) == c
                              testBottom    = fst (closestToCoord cs (fst c, (snd . snd $ edges)+1)) == c
                          in (testLeft || testRight || testTop || testBottom)

middleCoords :: [Coord] -> [Coord]
middleCoords acs = filter (\e -> fst e > minX && fst e < maxX && snd e > minY && snd e < maxY && not (testInfinity acs e edges)) acs
    where minX = fst . fst $ edges
          maxX = fst . snd $ edges
          minY = snd . fst $ edges
          maxY = snd . snd $ edges
          edges = getEdges acs

findClosest :: V.Vector Coord -> (Int -> Int) -> (Int -> Int -> Bool) -> ((Int, Int) -> Int) -> Coord -> Int -> Coord
findClosest cs fi fc fe c i = case (fc (fe (cs V.! i)) (fe c)) of
                False -> findClosest cs fi fc fe c (fi i)
                True -> cs V.! i

closestNeighbors :: [Coord] -> Coord -> (Coord, Coord, Coord, Coord)
closestNeighbors cs c = (x1, x2, y1, y2)
    where 
          idX = fromJust (V.elemIndex c vx)
          x1 = findClosest vx ((flip(-)) 1) (<) snd c (idX-1)
          x2 = findClosest vx ((+) 1) (<) snd c (idX+1)
          y1 = findClosest vx ((flip(-)) 1) (>) snd c (idX-1)
          y2 = findClosest vx ((+) 1) (>) snd c (idX+1)
          vx = V.fromList $ sort cs


closestToCoord :: [Coord] -> Coord -> (Coord, Distance)
closestToCoord cs co = foldl (\a c -> if (distBetween c co) < (snd a) then (c, distBetween c co) else (if (distBetween c co) == (snd a) then ((-1,-1),distBetween c co) else a)) ((-1,-1),(maxBound :: Int)) cs

isClosestToMe :: [Coord] -> Coord -> Coord -> Bool
isClosestToMe cs c me = if (fst (closestToCoord cs c) == me) then True else False

getMaxCoords :: (Coord, Coord, Coord, Coord) -> (Coord, Coord)
getMaxCoords (a, b, c, d) = ((minX, minY), (maxX, maxY))
    where minX = min (fst a) (fst c)
          maxX = max (fst b) (fst d)
          minY = min (snd a) (snd b)
          maxY = max (snd c) (snd d)

areaCovered :: [Coord] -> Coord -> Int
areaCovered cs c = foldl (\a x -> if isClosestToMe cs x c then a+1 else a) 0 cands 
    where
          cands = [(x,y) | x <- [fst . fst $ mc .. fst . snd $ mc], y <- [snd . fst $ mc .. snd . snd $ mc]]
          mc = getEdges cs

getMaxArea :: [Coord] -> Int
getMaxArea cs = foldl max 0 (map (areaCovered cs) (middleCoords cs))
