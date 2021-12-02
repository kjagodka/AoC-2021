updatePosition :: (Int, Int) -> (String, Int) -> (Int, Int)
updatePosition (depth, horizontal) ("up", distance) = (depth - distance, horizontal)
updatePosition (depth, horizontal) ("down", distance) = (depth + distance, horizontal)
updatePosition (depth, horizontal) ("forward", distance) = (depth, horizontal + distance)

main = do
  inp <- getContents
  let dat = map ((\[str, n] -> (str, (read :: String -> Int) n)) . words) . lines $ inp
      finalPosition = foldl updatePosition (0, 0) dat
  print . uncurry (*) $ finalPosition;
