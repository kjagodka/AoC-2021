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

composeWithItself :: Int -> (a -> a) -> (a -> a)
composeWithItself n f = iterate (f .) id !! n

main :: IO ()
main = do
  inp <- getContents
  let lanternfishInitPopulation = lanternfishPopFromList . map read . splitOn "," $ inp
      popAfter80Days = composeWithItself 80 updatePopulation lanternfishInitPopulation
      popAfter256Days = composeWithItself 256 updatePopulation lanternfishInitPopulation
  print . countFish $ popAfter80Days
  print . countFish $ popAfter256Days
