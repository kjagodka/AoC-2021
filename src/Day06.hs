import Data.List (elemIndices)
import qualified Data.Sequence as S
import Data.List.Split (splitOn)

newtype LanternfishPop = AgesPops (S.Seq Int)

count :: Eq a => [a] -> a -> Int
count list elem = length $ elemIndices elem list

lanternfishPopFromList :: [Int] -> LanternfishPop
lanternfishPopFromList fishList = AgesPops (S.fromFunction 9 $ count fishList)

countFish :: LanternfishPop -> Int
countFish (AgesPops counts) = sum counts

updatePopulation :: LanternfishPop -> LanternfishPop
updatePopulation (AgesPops counts) =
  let fertile = S.index counts 0
      others = S.drop 1 counts
   in AgesPops (S.insertAt 8 fertile . S.adjust' (+ fertile) 6 $ others)

main :: IO ()
main = do
  inp <- getContents
  let lanternfishInitPopulation = lanternfishPopFromList . map read . splitOn "," $ inp
      populationAfterNDays = iterate updatePopulation lanternfishInitPopulation
  print . countFish $ populationAfterNDays !! 80
  print . countFish $ populationAfterNDays !! 256
