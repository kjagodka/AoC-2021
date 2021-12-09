import Data.List.Split (splitOn)
import qualified Data.Sequence as S

type FishPopulation = S.Seq Int

count :: Eq a => [a] -> a -> Int
count list elem = length $ filter (== elem) list

fishListToPopulation :: [Int] -> FishPopulation
fishListToPopulation = S.fromFunction 9 . count

countFish :: FishPopulation -> Int
countFish = sum

updatePopulation :: FishPopulation -> FishPopulation
updatePopulation population =
  let reproducers = S.index population 0
      others = S.drop 1 population
   in S.adjust' (+ reproducers) 6 $ S.insertAt 8 reproducers others

main :: IO ()
main = do
  inp <- getContents
  let initPopulation = fishListToPopulation . map read $ splitOn "," inp
      populationAfterNDays = iterate updatePopulation initPopulation
  print . countFish $ populationAfterNDays !! 80
  print . countFish $ populationAfterNDays !! 256
