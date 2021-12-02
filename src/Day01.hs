countIncrements :: [Int] -> Int
countIncrements = countIncrements' 0
  where countIncrements' acc [] = acc
        countIncrements' acc [_] = acc
        countIncrements' acc (a:b:rest) =
          countIncrements' (acc + isGreater) (b:rest)
          where
            isGreater = if a < b
              then 1
              else 0

walkingSum :: Int -> [Int] -> [Int]
walkingSum = walkingSum' []
  where
    walkingSum' acc n list =
      if length list < n
        then reverse acc
        else walkingSum' (sum (take n list):acc) n (tail list)

main :: IO ()
main = do {
  inp <- getContents;
  let ints = map (read::String->Int) . lines $ inp
      sums = walkingSum 3 ints
  in print (countIncrements ints, countIncrements sums);
  return ()
}
