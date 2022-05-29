import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Tuple (swap)

type Point = (Int, Int)

type Sheet = S.Set Point

data Axis = X Int | Y Int

type Fold = Sheet -> Sheet

toTuple :: [a] -> (a, a)
toTuple (x : y : _) = (x, y)
toTuple _ = undefined

readPoint :: String -> Point
readPoint s = toTuple . map read $ splitOn [','] s

readAxis :: String -> Axis
readAxis s =
  let (axisS, distS) = toTuple $ splitOn ['='] s
      dist = read distS :: Int
   in case axisS of
        "fold along x" -> X dist
        "fold along y" -> Y dist
        _ -> undefined

flipPoint :: Axis -> Point -> Point
flipPoint (X dist) (x, y) =
  if x < dist
    then (x, y)
    else (dist - (x - dist), y)
flipPoint (Y dist) point = swap . flipPoint (X dist) . swap $ point

axisToFold :: Axis -> Sheet -> Sheet
axisToFold = S.map . flipPoint

parseInput :: String -> (Sheet, [Fold])
parseInput input =
  let (pointStrings, foldStrings) = toTuple . splitOn [[]] . lines $ input
      points = map readPoint pointStrings
      folds = map (axisToFold . readAxis) foldStrings
   in (S.fromList points, folds)

renderPixel :: Bool -> Char
renderPixel True = 'X'
renderPixel False = ' '

renderRow :: Sheet -> Int -> Int -> Int -> String
renderRow sheet colsFrom colsTo rowNo =
  [ renderPixel $ (`S.member` sheet) (col, rowNo)
    | col <- [colsFrom .. colsTo]
  ]

renderSheet :: Sheet -> String
renderSheet sheet = do
  let colsTo = S.findMax $ S.map fst sheet
      colsFrom = S.findMin $ S.map fst sheet
      rowsTo = S.findMax $ S.map snd sheet
      rowsFrom = S.findMin $ S.map snd sheet
   in unlines [renderRow sheet colsFrom colsTo row | row <- [rowsFrom .. rowsTo]]

part1 :: Sheet -> [Fold] -> IO ()
part1 sheet folds =
  print . S.size $ head folds sheet

part2 :: Sheet -> [Fold] -> IO ()
part2 sheet folds = do
  let composedFold = foldl1 (flip (.)) folds
  putStr . renderSheet . composedFold $ sheet

main :: IO ()
main = do
  input <- getContents
  let (sheet, folds) = parseInput input
  part1 sheet folds
  part2 sheet folds