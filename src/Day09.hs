import BasePrelude (on)
import Data.Char (digitToInt)
import Data.List (minimumBy, sortBy)
import Data.Maybe (fromJust, isJust, mapMaybe, maybe)
import qualified Data.Vector as V

type HeightMap = V.Vector (V.Vector Int)

type Coords = (Int, Int)

count :: Eq a => [a] -> a -> Int
count list elem = length $ filter (== elem) list

heightMapFromInts :: [[Int]] -> HeightMap
heightMapFromInts = V.map V.fromList . V.fromList

getHeight :: HeightMap -> Coords -> Maybe Int
getHeight hmap (row, col) = (>>=) (hmap V.!? row) (V.!? col)

isIndex :: HeightMap -> Coords -> Bool
isIndex hmap = isJust . getHeight hmap

neighbours :: HeightMap -> Coords -> [Coords]
neighbours hmap (row, col) =
  filter (isIndex hmap) [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]

isLocalMininum :: HeightMap -> Coords -> Bool
isLocalMininum hmap coords =
  let neigboursMinimum = minimum . mapMaybe (getHeight hmap) $ neighbours hmap coords
   in (fromJust . getHeight hmap $ coords) < neigboursMinimum

getIndices :: HeightMap -> [Coords]
getIndices hmap =
  let rows = V.length hmap
      cols = V.length $ hmap V.! 0
   in [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

findLocalMinima :: HeightMap -> [Coords]
findLocalMinima hmap = filter (isLocalMininum hmap) . getIndices $ hmap

isBasin :: HeightMap -> Coords -> Bool
isBasin hmap coords = Just 9 /= getHeight hmap coords

followGradient :: HeightMap -> Coords -> Coords
followGradient hmap coords
  | isLocalMininum hmap coords = coords
  | otherwise =
    followGradient hmap
      . minimumBy (compare `on` fromJust . getHeight hmap)
      $ neighbours hmap coords

part1 :: HeightMap -> IO ()
part1 hmap =
  let localMinima = findLocalMinima hmap
   in print . sum . map ((1 +) . fromJust . getHeight hmap) $ localMinima

part2 :: HeightMap -> IO ()
part2 hmap =
  let basinsCoordsList = filter (isBasin hmap) (getIndices hmap)
      mapped = map (followGradient hmap) basinsCoordsList
      localMinima = findLocalMinima hmap
      sizes = sortBy (flip compare). map (count mapped) $ localMinima
   in print . product . take 3 $ sizes

main :: IO ()
main = do
  stdin <- getContents
  let hmap = heightMapFromInts . map (map digitToInt) . lines $ stdin
      coordsList = getIndices hmap
      localMinima = findLocalMinima hmap
  part1 hmap
  part2 hmap