import Data.Char (isDigit)
import Data.List (nub, tails)
import Data.List.Split (splitOn)
import qualified Data.Map

data Point = Point Int Int deriving (Eq, Ord)

data Line = Line Point Point

lineToPoints :: Line -> [Point]
lineToPoints (Line (Point ax ay) (Point bx by)) =
  let dx = signum (bx - ax)
      dy = signum (by - ay)
      len = if dx /= 0 then dx * (bx - ax) else dy * (by - ay)
   in [Point (ax + i * dx) (ay + i * dy) | i <- [0 .. len]]

isDiagonal :: Line -> Bool
isDiagonal (Line (Point ax ay) (Point bx by)) = ax /= bx && ay /= by

addPointToMap :: Data.Map.Map Point Int -> Point -> Data.Map.Map Point Int
addPointToMap pointsMap point =
  Data.Map.insertWith (+) point 1 pointsMap

addLineToMap :: Data.Map.Map Point Int -> Line -> Data.Map.Map Point Int
addLineToMap pointsMap line =
  foldl addPointToMap pointsMap $ lineToPoints line

countDangerousPoints :: [Line] -> Int
countDangerousPoints = Data.Map.size . Data.Map.filter (>= 2) . foldl addLineToMap Data.Map.empty

main :: IO ()
main = do
  inp <- getContents
  let ventLines = map ((\[[a, b], [c, d]] -> Line (Point a b) (Point c d)) . map (map (read :: String -> Int) . splitOn ",") . splitOn " -> ") . lines $ inp
      horOrVertLines = filter (not . isDiagonal) ventLines
  print (countDangerousPoints horOrVertLines, countDangerousPoints ventLines)