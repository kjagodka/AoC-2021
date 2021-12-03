import Data.List ( foldl', elemIndices, transpose )
import Data.Char (digitToInt)

count :: Eq a => a -> [a] -> Int
count elem list = length $ elemIndices elem list

mostCommonBits :: [[Int]] -> [Int]
mostCommonBits lns = map (\col -> if count 1 col >= count 0 col then 1 else 0 ) $ transpose lns

leastCommonBits :: [[Int]] -> [Int]
leastCommonBits lns = map (1 - ) $ mostCommonBits lns

binaryToInt :: [Int] -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + x) 0

findRating :: ([[Int]] -> [Int]) -> [[Int]] -> [Int]
findRating bitSelector numbers = findRating' 0 numbers
  where
    len = length . head $ numbers
    findRating' _ [x] = x
    findRating' index numbers =
      let filterBit = bitSelector numbers !! index
      in findRating' ((index + 1) `mod` len) $ filter (\line -> line !! index == filterBit) numbers


main = do {
  dta <- getContents;
  let numbers = map (map digitToInt) $ lines dta
      gammaRate = (binaryToInt . mostCommonBits $ numbers) * (binaryToInt . leastCommonBits $ numbers)
      oxygenRating = binaryToInt $ findRating mostCommonBits numbers
      co2Rating = binaryToInt $ findRating leastCommonBits numbers
  in print (gammaRate, oxygenRating * co2Rating);
  }
