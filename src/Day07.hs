{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Functor (fmap)
import Data.List (sort)
import Language.LSP.Types.Lens (HasPositions (positions))

newtype Fuel = Fuel Int deriving (Eq, Ord, Show, Num)

newtype Position = Position Int deriving (Eq, Ord, Enum)

newtype Range = Range (Position, Position)

newtype Distance = Distance Int

distance :: Position -> Position -> Distance
distance (Position from) (Position to) = Distance (abs (to - from))

crabFuelUsage :: Distance -> Fuel
crabFuelUsage (Distance d) = Fuel ((d * (d + 1)) `div` 2)

standardFuelUsage :: Distance -> Fuel
standardFuelUsage (Distance d) = Fuel d

standardMetric :: Position -> Position -> Fuel
standardMetric from to = standardFuelUsage $ distance from to

crabMetric :: Position -> Position -> Fuel
crabMetric from to = crabFuelUsage $ distance from to

getRange :: [Position] -> Range
getRange positions = Range (minimum positions, maximum positions)

isSingleton :: Range -> Bool
isSingleton (Range (Position a, Position b)) = a == b

midPoint :: Range -> Position
midPoint (Range (Position a, Position b)) = Position ((a + b) `div` 2)

leftOf :: Range -> Position -> Range
leftOf (Range (from, _)) pos = Range (from, pos)

rightOf :: Range -> Position -> Range
rightOf (Range (_, to)) (Position pos) = Range (Position (pos + 1), to)

checkFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Position -> Fuel
checkFuelUsage metric startPositions meetingPos =
  sum $ map (metric meetingPos) startPositions

optimalPosition :: (Position -> Position -> Fuel) -> [Position] -> Range -> Position
optimalPosition metric startingPositions range
  | isSingleton range = mid
  | checkFuelUsage metric startingPositions mid
      > checkFuelUsage
        metric
        startingPositions
        ( succ mid
        ) =
    optimalPosition metric startingPositions $ rightOf range mid
  | otherwise = optimalPosition metric startingPositions $ leftOf range mid
  where
    mid = midPoint range

minimalFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Fuel
minimalFuelUsage metric positions =
  let range = getRange positions
      optimalPos = optimalPosition metric positions range
   in checkFuelUsage metric positions optimalPos

main :: IO ()
main = do
  stdin <- getContents
  let crabsPositions = map Position $ read ("[" ++ stdin ++ "]")
  print $ minimalFuelUsage standardMetric crabsPositions
  print $ minimalFuelUsage crabMetric crabsPositions