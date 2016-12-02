import System.IO
-- for part 2
import qualified Data.Set as Set

-- Types
data Direction = North | East | South | West deriving (Show)
data Coord2d a = Coord2d (a, a) deriving (Eq, Ord, Show)
unCoord2d :: Coord2d a -> (a, a)
unCoord2d (Coord2d x) = x

-- Input Processing
-- split into parts
splitOn :: (Char -> Bool) -> String -> [String]
splitOn predicate s = case dropWhile predicate s of
                      "" -> []
                      s' -> w : splitOn predicate s''
                            where (w, s'') = break predicate s'

splitComp :: (a -> b) -> (a -> b) -> (b -> b -> c) -> a -> c
splitComp f1 f2 comp a = comp (f1 a) (f2 a)

delimiter :: (Char -> Bool)
delimiter = splitComp (== ',') (== ' ') (||)

splitInput :: String -> [String]
splitInput = splitOn delimiter

-- parse direction and distance
readInt :: String -> Int
readInt = read

parseInstruction :: String -> (Char, Int)
parseInstruction (x:xs) = (x, readInt xs)

parseInput :: String -> [(Char, Int)]
parseInput = map parseInstruction . splitInput

-- Direction helper stuff
turnDir :: Char -> Direction -> Direction
turnDir c d = case c of
                'L' -> turnDir 'R' $ turnDir 'R' $ turnDir 'R' d
                'R' -> case d of
                         North -> East
                         East -> South
                         South -> West
                         West -> North

coordModify :: (Num a) => Direction -> Coord2d a -> a -> Coord2d a
coordModify East (Coord2d (x, y)) n = Coord2d (x + n, y)
coordModify West (Coord2d (x, y)) n = Coord2d (x - n, y)
coordModify North (Coord2d (x, y)) n = Coord2d (x, y + n)
coordModify South (Coord2d (x, y)) n = Coord2d (x, y - n)

taxiDistance (Coord2d (x, y)) = abs x + abs y

maybeTaxiDistance (Just (Coord2d (x, y))) = abs x + abs y
maybeTaxiDistance Nothing = -42

-- Calcualate Part 1
turnAndMove :: (Num a) => (Direction, Coord2d a) -> (Char, a) -> (Direction, Coord2d a) 
turnAndMove (oldDir, oldCoord) (turn, dist) = (newDir, coordModify newDir oldCoord dist) where newDir = turnDir turn oldDir

finalPos :: (Num a) => [(Char, a)] -> (Direction, Coord2d a)
finalPos = foldl turnAndMove (North, Coord2d (0, 0))

-- Calculate Part 2

distances :: (Num a, Eq a) => a -> [a]
distances 0 = []
distances x = x:distances (x - 1)

coordModifyMarkII :: (Num a, Eq a) => Direction -> Coord2d a -> a -> (Coord2d a, [Coord2d a])
coordModifyMarkII dir coord dist = (calc dist, map calc (distances dist)) where calc = coordModify dir coord

findIntersection :: (Ord a) => Set.Set a -> [a] -> Maybe a
findIntersection set = foldr isMember Nothing
                       where isMember x Nothing = case Set.member x set of
                                                False -> Nothing
                                                True -> Just x
                             isMember x (Just acc) = Just acc

turnAndMoveMarkII :: (Num a, Eq a, Ord a) => (Maybe (Coord2d a), Set.Set (Coord2d a), (Direction, Coord2d a)) -> (Char, a) -> (Maybe (Coord2d a), Set.Set (Coord2d a), (Direction, Coord2d a))
turnAndMoveMarkII (Nothing, prevVisited, (oldDir, oldCoord)) (turn, dist) = (finalDest, visited, (turnDir turn oldDir, newDest)) where
                                                                              (newDest, newVisited) = coordModifyMarkII (turnDir turn oldDir) oldCoord dist
                                                                              finalDest = findIntersection prevVisited newVisited 
                                                                              visited = Set.union prevVisited $ foldr (\x acc -> Set.insert x acc) Set.empty newVisited
turnAndMoveMarkII (Just coord, vis, result) _ = (Just coord, vis, result)

extractVisited (_, visited, _) = visited

extractDest (Just dest, _, _) = Just dest
extractDest (Nothing, _, _) = Nothing
finalPosMarkII :: (Num a, Eq a, Ord a) => [(Char, a)] -> Maybe (Coord2d a)
finalPosMarkII = extractDest . foldl turnAndMoveMarkII (Nothing, Set.singleton $ Coord2d (0, 0), (North, Coord2d (0, 0)))

finalVisited :: (Num a, Eq a, Ord a) => [(Char, a)] -> Set.Set (Coord2d a)
finalVisited = extractVisited . foldl turnAndMoveMarkII (Nothing, Set.singleton $ Coord2d (0, 0), (North, Coord2d (0, 0)))

main :: IO ()
main = do
  inFile <- openFile "inputFiles/day_1" ReadMode
  rawInput <- hGetContents inFile
  print $ taxiDistance . snd $ finalPos $ parseInput rawInput
  print $ maybeTaxiDistance $ finalPosMarkII $ parseInput rawInput
