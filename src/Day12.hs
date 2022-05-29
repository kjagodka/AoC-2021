import Data.Char (isLower, isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Retrie.PatternMap.Class as M

data Cave = SmallCave String | BigCave String | CaveStart | CaveEnd
  deriving (Eq, Ord)

type CaveNeighboursMap = M.Map Cave [Cave]

type CaveGraph = Cave -> [Cave]

addEdge :: CaveNeighboursMap -> [Cave] -> CaveNeighboursMap
addEdge g [caveA, caveB] =
  let g' = M.insertWith (++) caveA [caveB] g
      g'' = M.insertWith (++) caveB [caveA] g'
   in g''

getGraph :: CaveNeighboursMap -> CaveGraph
getGraph caveMap cave = M.findWithDefault [] cave caveMap

stringToCave :: String -> Cave
stringToCave "start" = CaveStart
stringToCave "end" = CaveEnd
stringToCave s = case s of
  c : _ | isLower c -> SmallCave s
  c : _ | isUpper c -> BigCave s

countPaths :: CaveGraph -> S.Set Cave -> Cave -> Int
countPaths graph visited current = case current of
  CaveEnd -> 1
  BigCave _ -> sum $ map (countPaths graph visited) (graph current)
  _ | S.member current visited -> 0
  _ ->
    let visited' = S.insert current visited
     in sum $ map (countPaths graph visited') (graph current)

part1 :: CaveGraph -> IO ()
part1 graph = do
  print $ countPaths graph S.empty CaveStart

main :: IO ()
main = do
  stdin <- getContents
  let edges = map (map stringToCave . splitOn "-") $ lines stdin
  let graph = getGraph . foldl addEdge M.empty $ edges
  part1 graph