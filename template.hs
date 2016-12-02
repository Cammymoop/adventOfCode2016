import System.IO

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_x" ReadMode
  rawInput <- hGetContents inFile
