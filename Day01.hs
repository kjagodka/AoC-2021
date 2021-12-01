
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

main :: IO ()
main = do {
  inp <- getContents;
  let ints = map (read::String->Int) . lines $ inp
  in print (countIncrements ints);
  return ()
}
