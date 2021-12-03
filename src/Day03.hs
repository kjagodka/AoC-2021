import Data.List

count :: Eq a => a -> [a] -> Int
count elem list = length $ elemIndices elem list

digitListToInt :: Num a => a -> [a] -> a
digitListToInt base digits = fst $ foldr (\digit (acc, pow) -> (acc + pow * digit, pow * base)) (0, 1) digits

binaryListToInt :: [Integer] -> Integer
binaryListToInt = digitListToInt 2

main = do {
  dta <- getContents;
  let
    columns = map (map (read . pure :: Char->Int)) $ transpose $ lines dta;
    len = length columns
    mostCommonBits = map (\col -> if count 1 col > count 0 col then 1 else 0 ) columns
    leastCommonBits = map (1 -) mostCommonBits

  in print $ binaryListToInt mostCommonBits * binaryListToInt leastCommonBits
  }
