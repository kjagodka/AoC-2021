import Data.List.Split ( splitOn )
import Data.List ( inits, minimumBy, maximumBy, transpose )
import Data.Ord (comparing)

countCommonElements :: Eq a => [a] -> [a] -> Int
countCommonElements xs ys =
  length $ filter (`elem` ys) xs

isBoardWinning :: Eq a => [[a]] -> [a] -> Bool
isBoardWinning board numbers =
  any (\row -> boardSize == countCommonElements numbers row) board ||
  any (\col -> boardSize == countCommonElements numbers col) (transpose board)
  where boardSize = length . head $ board

--returns number of picked numbers needed to complete some row or column or given board
numbersToWin :: Eq a => [[a]] -> [a] -> Int
numbersToWin board numbers =
  let winningLists = filter (isBoardWinning board) $ inits numbers
  in length . head $ winningLists

findWinningBoard :: Eq a => [[[a]]] -> [a] -> [[a]]
findWinningBoard boards numbers =
  minimumBy (comparing (`numbersToWin` numbers)) boards

findLoosingBoard :: Eq a => [[[a]]] -> [a] -> [[a]]
findLoosingBoard boards numbers =
  maximumBy (comparing (`numbersToWin` numbers)) boards

remainingNumbers :: (Eq a) => [[a]] -> [a] -> [[a]]
remainingNumbers board numbers = 
  map (filter (`notElem` numbers)) board

main :: IO ()
main = do
  inp <- getContents
  let numbers = map read $ splitOn "," $ head $ lines inp :: [Int]
      boards = splitOn [[]] $ map (map read . words) . tail . tail $ lines inp :: [[[Int]]]
      winningBoard = findWinningBoard boards numbers
      winningBoardNumbers = take (numbersToWin winningBoard numbers) numbers
      answer1 = last winningBoardNumbers * (sum . concat $ remainingNumbers winningBoard winningBoardNumbers)
      loosingBoard = findLoosingBoard boards numbers
      loosingBoardNumbers = take (numbersToWin loosingBoard numbers) numbers
      answer2 = last loosingBoardNumbers * (sum . concat $ remainingNumbers loosingBoard loosingBoardNumbers)

  print (answer1, answer2)