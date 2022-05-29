import Data.Char (isLower, isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Retrie.PatternMap.Class as M

data Cave = CaveSmall String | CaveBig String | CaveStart | CaveEnd
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
  c : _ | isLower c -> CaveSmall s
  c : _ | isUpper c -> CaveBig s
  _ -> undefined

countPaths :: CaveGraph -> S.Set Cave -> Bool -> Cave -> Int
countPaths graph visited canVisitAgain current = case current of
  CaveEnd -> 1
  CaveBig _ -> sum $ map (countPaths graph visited canVisitAgain) neighbors
  CaveSmall _
    | S.member current visited && canVisitAgain ->
      sum $ map (countPaths graph visited False) neighbors
  _ | S.member current visited -> 0
  _ -> sum $ map (countPaths graph visited' canVisitAgain) neighbors
  where
    neighbors = graph current
    visited' = S.insert current visited

part1 :: CaveGraph -> IO ()
part1 graph = do
  print $ countPaths graph S.empty True CaveStart

part2 :: CaveGraph -> IO ()
part2 graph = do
  print $ countPaths graph S.empty False CaveStart

main :: IO ()
main = do
  stdin <- getContents
  let edges = map (map stringToCave . splitOn "-") $ lines stdin
  let graph = getGraph . foldl addEdge M.empty $ edges
  part1 graph
  part2 graph