import Data.List.Split (splitOn)
import qualified Data.Sequence as S

newtype FishPopulation = FishPopulation (S.Seq Int)

count :: Eq a => [a] -> a -> Int
count list elem = length $ filter (== elem) list

fishListToPopulation :: [Int] -> FishPopulation
fishListToPopulation fishList = FishPopulation (S.fromFunction 9 $ count fishList)

countFish :: FishPopulation -> Int
countFish (FishPopulation counts) = sum counts

updatePopulation :: FishPopulation -> FishPopulation
updatePopulation (FishPopulation counts) =
  let reproducers = S.index counts 0
      others = S.drop 1 counts
   in FishPopulation (S.adjust' (+ reproducers) 6 $ S.insertAt 8 reproducers others)

main :: IO ()
main = do
  inp <- getContents
  let initPopulation = fishListToPopulation . map read $ splitOn "," inp
      populationAfterNDays = iterate updatePopulation initPopulation
  print . countFish $ populationAfterNDays !! 80
  print . countFish $ populationAfterNDays !! 256
