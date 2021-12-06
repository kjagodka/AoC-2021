import Data.List (elemIndices)
import Data.List.Split (splitOn)

newtype LanternfishPop = AgesPops [Int] deriving (Show)

count :: Eq a => [a] -> a -> Int
count list elem = length $ elemIndices elem list

lanternfishPopFromList :: [Int] -> LanternfishPop
lanternfishPopFromList fishList = AgesPops $ map (count fishList) [0 .. 8]

countFish :: LanternfishPop -> Int
countFish (AgesPops counts) = sum counts

updatePopulation :: LanternfishPop -> LanternfishPop
updatePopulation (AgesPops counts) =
  let fertile = head counts --fish with internal counter 0
      otherAdults = take 6 $ tail counts --fish with internal counter 1..5
      [fish7, fish8] = drop 7 counts --fish with internal counter 7..8
      newPop = otherAdults ++ [fertile + fish7, fish8, fertile]
   in AgesPops newPop

main :: IO ()
main = do
  inp <- getContents
  let lanternfishInitPopulation = lanternfishPopFromList . map read . splitOn "," $ inp
      populationAfterNDays = iterate updatePopulation lanternfishInitPopulation
  print . countFish $ populationAfterNDays !! 80
  print . countFish $ populationAfterNDays !! 256
