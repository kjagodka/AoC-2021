import Data.Either (lefts, rights)
import Data.Foldable (foldlM)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

type BracketStack = String

type Error = Char

matchingBracket :: Char -> Maybe Char
matchingBracket = flip lookup [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

pushBracket :: BracketStack -> Char -> Either Char BracketStack
pushBracket stack c = case stack of
  _ | c `elem` "([{<" -> Right $ c : stack
  s : rest | c `elem` ")]}>" && matchingBracket s == Just c -> Right rest
  _ -> Left c

stackFromString :: String -> Either Char BracketStack
stackFromString = foldlM pushBracket []

errorValue :: Char -> Int
errorValue ')' = 3
errorValue ']' = 57
errorValue '}' = 1197
errorValue '>' = 25137

autocompleteValue :: Char -> Int
autocompleteValue ')' = 1
autocompleteValue ']' = 2
autocompleteValue '}' = 3
autocompleteValue '>' = 4

part1 :: [Char] -> Int
part1 = sum . map errorValue

median :: [Int] -> Int
median list =
  let len = length list
      sorted = sort list
   in sort list !! (len `div` 2)

part2 :: [BracketStack] -> Int
part2 = median . map (foldl (\acc c -> (autocompleteValue . fromJust . matchingBracket $ c) + 5 * acc) 0)

main :: IO ()
main = do
  stdin <- getContents
  let bracketStacks = map stackFromString . lines $ stdin
  print . part1 . lefts $ bracketStacks
  print . part2 . rights $ bracketStacks