import System.IO

-- utility functions
readInt :: String -> Int
readInt = read

processInput = map $ (map readInt) . words

-- Check for/count valid traingles

isValidTriangle sides = ((<) . (*2) . maximum) sides $ sum sides 

countValidTriangles = foldr (\x acc -> if isValidTriangle x then acc + 1 else acc) 0

-- process input mark II

flipMatrix (a:b:c:_) = zipWith3 (\x y z -> x:y:[z]) a b c

fixInput [] = []
fixInput xs = (++) (flipMatrix $ take 3 $ xs) $ fixInput $ drop 3 $ xs

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_3" ReadMode
  rawInput <- hGetContents inFile
  print $ countValidTriangles $ processInput $ lines rawInput
  print "second count:"
  print $ countValidTriangles $ fixInput . processInput $ lines rawInput
