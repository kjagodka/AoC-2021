import Data.Either (lefts, rights)
import Data.Foldable (foldlM)
import Data.List (elemIndex, sort)
import Data.Maybe (mapMaybe)

type BracketStack = String

type Error = Char

matchingBracket :: Char -> Maybe Char
matchingBracket = flip lookup [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

pushBracket :: BracketStack -> Char -> Either Error BracketStack
pushBracket stack c = case stack of
  _ | c `elem` "([{<" -> Right $ c : stack
  s : rest | c `elem` ")]}>" && matchingBracket s == Just c -> Right rest
  _ -> Left c

stackFromString :: String -> Either Char BracketStack
stackFromString = foldlM pushBracket []

errorValue :: Error -> Maybe Int
errorValue = flip lookup [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

autocompleteValue :: Char -> Maybe Int
autocompleteValue = flip lookup [(')', 1), (']', 2), ('}', 3), ('>', 4)]

part1 :: [Error] -> Int
part1 = sum . mapMaybe errorValue

median :: [Int] -> Int
median list =
  let len = length list
      sorted = sort list
   in sort list !! (len `div` 2)

part2 :: [BracketStack] -> Int
part2 = median . mapMaybe (foldlM (\acc c -> matchingBracket c >>= autocompleteValue >>= \n -> Just (5*acc + n)) 0)

main :: IO ()
main = do
  stdin <- getContents
  let bracketStacks = map stackFromString . lines $ stdin
  print . part1 . lefts $ bracketStacks
  print . part2 . rights $ bracketStacks