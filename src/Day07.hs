type Fuel = Int

type Position = Int

type Range = (Position, Position)

type Distance = Int

distance :: Position -> Position -> Distance
distance from to = abs (to - from)

standardMetric :: Position -> Position -> Fuel
standardMetric from to = distance from $ to

crabMetric :: Position -> Position -> Fuel
crabMetric from to = (d * (d + 1)) `div` 2
  where
    d = distance from to

getRange :: [Position] -> Range
getRange positions = (minimum positions, maximum positions)

isSingleton :: Range -> Bool
isSingleton (a, b) = a == b

midPoint :: Range -> Position
midPoint (a, b) = (a + b) `div` 2

leftOf :: Range -> Position -> Range
leftOf (from, _) pos = (from, pos)

rightOf :: Range -> Position -> Range
rightOf (_, to) pos = (pos + 1, to)

checkFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Position -> Fuel
checkFuelUsage metric startPositions meetingPos =
  sum $ map (metric meetingPos) startPositions

optimalPosition :: (Position -> Position -> Fuel) -> [Position] -> Range -> Position
optimalPosition metric startingPositions range
  | isSingleton range = mid
  | diff > 0 = optimalPosition metric startingPositions $ rightOf range mid
  | otherwise = optimalPosition metric startingPositions $ leftOf range mid
  where
    mid = midPoint range
    diff =
      checkFuelUsage metric startingPositions mid
        - checkFuelUsage metric startingPositions (succ mid)

minimalFuelUsage :: (Position -> Position -> Fuel) -> [Position] -> Fuel
minimalFuelUsage metric positions =
  let range = getRange positions
      optimalPos = optimalPosition metric positions range
   in checkFuelUsage metric positions optimalPos

main :: IO ()
main = do
  stdin <- getContents
  let crabsPositions = read ("[" ++ stdin ++ "]")
  print $ minimalFuelUsage standardMetric crabsPositions
  print $ minimalFuelUsage crabMetric crabsPositions