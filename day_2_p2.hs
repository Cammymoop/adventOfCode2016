import System.IO
import Data.Char as Char

-- Day 2 part 2
--
-- Keypad now looks like
--
--  ~ 1
--   234
--  56789
--   ABC
--    D
--
-- ~ is (0, 0)

-- restrict the coords to the diamond shaped keypad
-- this specifically works with only diamond shaped keypads
isCoordValid :: (Num a, Ord a) => (a, a) -> Bool
isCoordValid (x, y) = ((x+y) > 1) && ((x+y) < 7) && ((x+(4-y)) > 1) && ((x+(4-y)) < 7) -- Math Magic!

numToChar :: (Integral a) => a -> Char
numToChar = Char.toUpper . Char.intToDigit . fromIntegral

keypadCharacter :: (Integral a) => (a, a) -> Char
keypadCharacter (x, y) = case y of
                           0 -> '1'
                           1 -> numToChar $ x + 1
                           2 -> numToChar $ x + 5
                           3 -> numToChar $ x + 9
                           4 -> 'D'

calcDelta :: (Num a) => Char -> ((a, a) -> (a, a))
calcDelta c = case c of
                'R' -> \(x, y) -> (x + 1, y)
                'L' -> \(x, y) -> (x - 1, y)
                'U' -> \(x, y) -> (x, y - 1)
                'D' -> \(x, y) -> (x, y + 1)
calcDeltaWrap coord char = if isCoordValid newCoord then newCoord else coord 
                            where newCoord = calcDelta char coord

nextNumber :: (Ord a, Num a) => (a, a) -> String -> (a, a)
nextNumber = foldl calcDeltaWrap

nextNumber' :: (Ord a, Integral a) => ((a, a), String) -> String -> ((a, a), String)
nextNumber' acc = (\coord -> (coord, snd acc ++ [keypadCharacter coord])) . (nextNumber $ fst acc)

-- Starting at coordinate 2, 2
calcCode = foldl nextNumber' ((2, 2), "")

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_2" ReadMode
  rawInput <- hGetContents inFile
  print $ calcCode $ lines rawInput
