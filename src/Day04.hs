import Data.List (inits, maximumBy, minimumBy, transpose, find)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Maybe (isNothing, catMaybes, fromJust)

newtype Board = Board [[Maybe Int]]

countCommonElements :: Eq a => [a] -> [a] -> Int
countCommonElements xs ys =
  length $ filter (`elem` ys) xs

isBoardWinning :: Board -> Bool
isBoardWinning (Board board) =
  any (all isNothing) board ||
    any (all isNothing) (transpose board)

crossNumber :: Board -> Int -> Board
crossNumber (Board board) n =
  Board $ map (map (\num -> if num == Just n then Nothing else num)) board

--returns number of picked numbers needed to complete some row or column on given board
numbersToWin :: Board -> [Int] -> Int
numbersToWin board numbers =
  let boardStates = foldl (\(prevState:states) n -> crossNumber prevState n:prevState:states) [board] numbers
   in length . filter (not . isBoardWinning) $ boardStates


findWinningBoard :: [Board] -> [Int] -> Board
findWinningBoard boards numbers =
  minimumBy (comparing (`numbersToWin` numbers)) boards

findLoosingBoard :: [Board] -> [Int] -> Board
findLoosingBoard boards numbers =
  maximumBy (comparing (`numbersToWin` numbers)) boards

remainingNumbers ::  Board -> [Int] -> [Int]
remainingNumbers board numbers =
  let (Board b) = foldl crossNumber board numbers
   in Data.Maybe.catMaybes . concat $ b

main :: IO ()
main = do
  inp <- getContents
  let numbers = map read . splitOn "," . head . lines $ inp :: [Int]
      boards = map Board $ splitOn [[]] $ map (map (Just . read) . words) . tail . tail $ lines inp :: [Board]
      winningBoard = findWinningBoard boards numbers
      winningBoardNumbers = take (numbersToWin winningBoard numbers) numbers
      answer1 = last winningBoardNumbers * (sum . remainingNumbers winningBoard $ winningBoardNumbers)
      loosingBoard = findLoosingBoard boards numbers
      loosingBoardNumbers = take (numbersToWin loosingBoard numbers) numbers
      answer2 = last loosingBoardNumbers * (sum . remainingNumbers loosingBoard $ loosingBoardNumbers)

  print (answer1, answer2)