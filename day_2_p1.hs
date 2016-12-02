import System.IO

-- Day 2 part 1

-- utility functions
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

wrap :: (Ord a) => a -> a -> a -> a
wrap min max val = if' (val > max) max $ if' (val < min) min val

-- I'm going to set up the coordinates like this:
--
-- 1,0 | 2,0 | 3,0
-- 1,1 | 2,1 | 3,1
-- 1,2 | 2,2 | 3,2
--
-- So that the number on the pad is x+3y
numberFromCoordinate = flip $ (+) . (*3)
numberFromCoordPair (x, y) = numberFromCoordinate x y

wrapCoordinate (x, y) = (wrap 1 3 x, wrap 0 2 y)

calcDelta :: (Num a) => Char -> ((a, a) -> (a, a))
calcDelta c = case c of
                'R' -> \(x, y) -> (x + 1, y)
                'L' -> \(x, y) -> (x - 1, y)
                'U' -> \(x, y) -> (x, y - 1)
                'D' -> \(x, y) -> (x, y + 1)
calcDeltaWrap char = wrapCoordinate . (calcDelta char)

nextNumber :: (Ord a, Num a) => (a, a) -> String -> (a, a)
nextNumber = foldl (flip calcDeltaWrap)

nextNumber' acc = (\coord -> (coord, snd acc ++ [numberFromCoordPair coord])) . (nextNumber $ fst acc)

calcCode = foldl nextNumber' ((2, 1), [])

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_2" ReadMode
  rawInput <- hGetContents inFile
  print $ calcCode $ lines rawInput
