import Data.List (find, permutations, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)

data Status = On | Off

newtype Display = Display [Status]

statusFromBool :: Bool -> Status
statusFromBool True = On
statusFromBool False = Off

displayToInt :: Display -> Maybe Int
displayToInt (Display [On, On, On, Off, On, On, On]) = Just 0
displayToInt (Display [Off, Off, On, Off, Off, On, Off]) = Just 1
displayToInt (Display [On, Off, On, On, On, Off, On]) = Just 2
displayToInt (Display [On, Off, On, On, Off, On, On]) = Just 3
displayToInt (Display [Off, On, On, On, Off, On, Off]) = Just 4
displayToInt (Display [On, On, Off, On, Off, On, On]) = Just 5
displayToInt (Display [On, On, Off, On, On, On, On]) = Just 6
displayToInt (Display [On, Off, On, Off, Off, On, Off]) = Just 7
displayToInt (Display [On, On, On, On, On, On, On]) = Just 8
displayToInt (Display [On, On, On, On, Off, On, On]) = Just 9
displayToInt _ = Nothing

isNumber :: Display -> Bool
isNumber = isJust . displayToInt

displayPermutations :: Display -> [Display]
displayPermutations (Display segments) = map Display $ permutations segments

displayFromString :: String -> Display
displayFromString s = Display $ map (\n -> statusFromBool $ elem (iterate succ 'a' !! n) s) [0 .. 6]

decodeDisplays :: [Display] -> [Int]
decodeDisplays displays =
  let permutations = transpose $ map displayPermutations displays
      checkPermuatation :: [Display] -> Bool
      checkPermuatation = all isNumber
      findPermutation :: [[Display]] -> [Display]
      findPermutation perms = fromJust . find checkPermuatation $ perms
      len = length displays :: Int
   in drop (len - 4) . map (fromJust . displayToInt) . findPermutation $ permutations

digitsToInt :: Int -> [Int] -> Int
digitsToInt base = foldl (\num digit -> base * num + digit) 0

part1 :: [[Int]] -> IO ()
part1 = print . length . filter (`elem` [1, 4, 7, 8]) . concat

part2 :: [[Int]] -> IO ()
part2 = print . sum . map (digitsToInt 10)

main :: IO ()
main = do
  stdin <- getContents
  let displays = map (map displayFromString . words . concat . splitOn " |") . lines $ stdin
      outputValues = map decodeDisplays displays
  part1 outputValues
  part2 outputValues