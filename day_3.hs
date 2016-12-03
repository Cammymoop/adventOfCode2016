import System.IO

-- utility functions
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate s = case dropWhile predicate s of
                      "" -> []
                      s' -> w : splitOn predicate s''
                            where (w, s'') = break predicate s'

splitOnSpace = splitOn (== ' ')

readInt :: String -> Int
readInt = read

processInput = map $ (map readInt) . splitOnSpace

greatest = foldr max 0

-- Check for/count valid traingles

isValidTriangle sides = ((<) . (*2) . greatest) sides $ sum sides 

countValidTriangles = foldr (\x acc -> if isValidTriangle x then acc + 1 else acc) 0

-- process input mark II

flipMatrix :: [[a]] -> [[a]]
flipMatrix (a:b:c:_) = zipWith3 (\x y z -> x:y:[z]) a b c

fixInput [] = []
fixInput lines = (++) (flipMatrix $ take 3 $ lines) $ fixInput $ drop 3 $ lines

processInputMarkII = fixInput . processInput

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_3" ReadMode
  rawInput <- hGetContents inFile
  print $ countValidTriangles $ processInput $ lines rawInput
  print "second count:"
  print $ countValidTriangles $ processInputMarkII $ lines rawInput
