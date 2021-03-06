module Reduce where

import qualified Data.Vector.Unboxed as V
import Data.Char (ord, toUpper)

isOpposite :: Char -> Char -> Bool
isOpposite a b = abs (ord a - ord b) == 32

type Polymer = V.Vector Char

testV :: Polymer
testV = V.fromList "dabAcCaCBAcCcaDA"

mkPolymer :: String -> Polymer
mkPolymer s = V.fromList s

step :: (Polymer, Polymer) -> (Polymer, Polymer)
step (a,b)
    | (V.length b >= 2) = if (isOpposite (b V.! 0) (b V.! 1))
                            then (a,(V.drop 2 b))
                          else (V.snoc a (b V.! 0),(V.drop 1 b))
                          
    | (V.length b == 1) = (V.snoc a (b V.! 0), V.drop 1 b)

reductionRound :: Polymer -> Polymer
reductionRound a = doIt (V.empty, a)
               where doIt (a,b) = case (V.length b == 0) of
                                   True    -> a
                                   False   -> doIt (step (a,b))

reduce :: Polymer -> Polymer
reduce a = doIt a (V.length a)
    where doIt a l = let r = reductionRound a in 
                        case (l == (V.length r)) of
                            True    -> a
                            False   -> doIt r (V.length r)


removeElement :: Polymer -> Char -> Polymer
removeElement p c = V.filter (\e -> toUpper e /= toUpper c) p

simWOElem :: Polymer -> Char -> Int
simWOElem p c = V.length . reduce $ removeElement p c

getShortest :: Polymer -> [Char] -> Int
getShortest p cs = doIt p cs (maxBound :: Int)
    where doIt p (c:cs) m = let mm = simWOElem p c in
                                if (mm < m) then doIt p cs mm else doIt p cs m
          doIt p [] m = m
