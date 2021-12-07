{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Functor (fmap)
import Data.List (sort)

newtype Fuel = Fuel Int deriving (Eq, Ord, Num, Show)
newtype Position = Position Int deriving (Eq, Ord, Enum)
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

checkFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Position -> Fuel
checkFuelUsage metric startPositions meetingPos =
  sum $ map (metric meetingPos) startPositions

minimalFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Fuel
minimalFuelUsage metric startingPositions =
  minimum $ map (checkFuelUsage metric startingPositions) [min .. max]
  where
    min = minimum startingPositions
    max = maximum startingPositions

main :: IO ()
main = do
  stdin <- getContents
  let crabsPositions = map Position $ read ("[" ++ stdin ++ "]")
  print $ minimalFuelUsage standardMetric crabsPositions
  print $ minimalFuelUsage crabMetric crabsPositions