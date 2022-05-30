import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Tuple (swap)

type Point = (Int, Int)

newtype Sheet = Sheet (S.Set Point)

data Axis = X Int | Y Int

type Fold = Sheet -> Sheet

instance Show Sheet where
  show (Sheet s) = unlines $ map showRow rows
    where
      cols = [0 .. S.findMax $ S.map fst s] :: [Int]
      rows = [0 .. S.findMax $ S.map snd s] :: [Int]

      showRow :: Int -> String
      showRow row = map (showPixel . getPixel row) cols

      getPixel :: Int -> Int -> Bool
      getPixel row col = S.member (col, row) s

      showPixel :: Bool -> Char
      showPixel True = 'X'
      showPixel False = ' '

toTuple :: [a] -> (a, a)
toTuple [x, y] = (x, y)
toTuple _ = undefined

readPoint :: String -> Point
readPoint s = toTuple . map read $ splitOn [','] s

pointsToSheet :: [Point] -> Sheet
pointsToSheet = Sheet . S.fromList

sheetSize :: Sheet -> Int
sheetSize (Sheet s) = S.size s

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
axisToFold axis (Sheet s) = Sheet $ S.map (flipPoint axis) s

parseInput :: String -> (Sheet, [Fold])
parseInput input =
  let (pointStrings, foldStrings) = toTuple . splitOn [[]] . lines $ input
      points = map readPoint pointStrings
      folds = map (axisToFold . readAxis) foldStrings
   in (pointsToSheet points, folds)

part1 :: Sheet -> [Fold] -> IO ()
part1 sheet folds =
  print . sheetSize $ head folds sheet

part2 :: Sheet -> [Fold] -> IO ()
part2 sheet folds = do
  let composedFold = foldl1 (flip (.)) folds
  print . composedFold $ sheet

main :: IO ()
main = do
  input <- getContents
  let (sheet, folds) = parseInput input
  part1 sheet folds
  part2 sheet folds