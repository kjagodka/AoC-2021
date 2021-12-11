import Data.Char (digitToInt)
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)

type Coords = (Int, Int)

type Octopuses = M.Map Coords Int

getIndices :: Octopuses -> [Coords]
getIndices = M.keys

getEnergy :: Octopuses -> Coords -> Int
getEnergy octopuses = fromJust . flip M.lookup octopuses

updateOct :: Octopuses -> Coords -> Int -> Octopuses
updateOct oct coords e = M.insert coords e oct

neighbours :: Octopuses -> Coords -> [Coords]
neighbours octopuses (r, c) =
  filter (`M.member` octopuses) [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1), (r, c + 1), (r + 1, c + 1), (r + 1, c), (r + 1, c - 1), (r, c - 1)]

update :: (Octopuses, Int) -> (Octopuses, Int)
update (octopuses, _) = go (octopuses, 0) $ getIndices octopuses
  where
    go :: (Octopuses, Int) -> [Coords] -> (Octopuses, Int)
    go (oct, count) [] = (M.map (\energy -> if energy >= 10 then 0 else energy) oct, count) --reset energy of glowing octopus to 0 after updating all octopus
    go (oct, count) (coord : rest) =
      if energy == 10
        then go (newOct, count + 1) (neighbours oct coord ++ rest)
        else go (newOct, count) rest
      where
        energy = 1 + getEnergy oct coord
        newOct = updateOct oct coord energy

readOctopus :: String -> Octopuses
readOctopus s =
  let lns = map (map digitToInt) . lines $ s
      rows = length lns
      cols = length . head $ lns
      keys = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]
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