import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Char (digitToInt)
import Data.List (findIndex)

type Coords = (Int, Int)

type Octopuses = M.Map Coords Int

getIndices :: Octopuses -> [Coords]
getIndices = M.keys

getEnergy :: Octopuses -> Coords -> Int
getEnergy octopuses = fromJust . flip M.lookup octopuses

updateEnergy :: Octopuses -> Coords -> Int -> Octopuses
updateEnergy oct coords e = M.insert coords e oct

isIndex :: Octopuses -> Coords -> Bool
isIndex = flip M.member

neighbours :: Octopuses -> Coords -> [Coords]
neighbours octopuses (r, c) =
  filter (isIndex octopuses) [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1), (r, c + 1), (r + 1, c + 1), (r + 1, c), (r + 1, c - 1), (r, c - 1)]

update :: (Octopuses, Int) -> (Octopuses, Int)
update (octopuses, _) = resetGlowing . go (octopuses, 0) $ getIndices octopuses
  where
    go :: (Octopuses, Int) -> [Coords] -> (Octopuses, Int)
    go (oct, count) [] = (oct, count)
    go (oct, count) (coord : rest) = if energy == 9
      then go (updateEnergy oct coord (energy + 1), count + 1) (neighbours oct coord ++ rest)
      else go (updateEnergy oct coord (energy + 1), count) rest
      where energy = getEnergy oct coord
    resetGlowing :: (Octopuses, Int) -> (Octopuses, Int)
    resetGlowing (oct, count) = (M.map (\energy -> if energy > 9 then 0 else energy) oct, count)

readOctopus :: String -> Octopuses
readOctopus s = 
  let lns = map (map digitToInt) . lines $ s
      rows = length lns
      cols = length . head $ lns
      keys = [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
   in M.fromAscList . zip keys $ concat lns

part1 :: Octopuses -> Int
part1 oct = sum . map snd . take 100 . tail . iterate update $ (oct, 0)

part2 :: Octopuses -> Int
part2 oct = fromJust . findIndex ((100 ==) . snd) . iterate update $ (oct, 0)

main :: IO ()
main = do
  stdin <- getContents 
  let octopuses = readOctopus stdin
  print . part1 $ octopuses
  print . part2 $ octopuses