import Data.List ( foldl', elemIndices, transpose )
import Data.Char (digitToInt)

count :: Eq a => a -> [a] -> Int
count elem list = length $ elemIndices elem list

mostCommonBits :: [[Char]] -> [Char]
mostCommonBits lns = map (\col -> if count '1' col >= count '0' col then '1' else '0' ) $ transpose lns

leastCommonBits :: [[Char]] -> [Char]
leastCommonBits lns = map (\digit -> if digit == '1' then '0' else '1' ) $ mostCommonBits lns

binaryToInt :: [Char] -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

findRating :: ([[Char]] -> [Char]) -> [[Char]] -> [Char]
findRating bitSelector lns = findRating' 0 lns
  where
    len = length . head $ lns
    findRating' _ [x] = x
    findRating' index lns =
      let filterBit = bitSelector lns !! index
      in findRating' ((index + 1) `mod` len) $ filter (\line -> line !! index == filterBit) lns


main = do {
  dta <- getContents;
  let numbers = lines dta
      gammaRate = (binaryToInt . mostCommonBits $ numbers) * (binaryToInt . leastCommonBits $ numbers)
      oxygenRating = binaryToInt $ findRating mostCommonBits numbers
      co2Rating = binaryToInt $ findRating leastCommonBits numbers
  in print (gammaRate, oxygenRating * co2Rating)
  }
