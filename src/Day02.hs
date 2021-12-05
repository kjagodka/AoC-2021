updatePositionA :: (Int, Int) -> (String, Int) -> (Int, Int)
updatePositionA (depth, horizontal) command = case command of
  ("up", distance) -> (depth + distance, horizontal)
  ("down", distance) -> (depth - distance, horizontal)
  ("forward", distance) -> (depth, horizontal + distance)

updatePositionB :: ((Int, Int), Int) -> (String, Int) -> ((Int, Int), Int)
updatePositionB ((depth, horizontal), aim) command = case command of
  ("up", angle) -> ((depth, horizontal), aim - angle)
  ("down", angle) -> ((depth, horizontal), aim + angle)
  ("forward", distance) -> ((depth + distance * aim, horizontal + distance), aim)

main = do
  inp <- getContents
  let pardedData = map ((\[str, n] -> (str, read n)) . words) . lines $ inp :: [(String, Int)]
      finalPositionA = foldl updatePositionA (0, 0) pardedData
      finalPositionB = foldl updatePositionB ((0, 0), 0) pardedData
  print . uncurry (*) $ finalPositionA
  print . uncurry (*) $ fst finalPositionB
