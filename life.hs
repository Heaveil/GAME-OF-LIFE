import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Set qualified as Set
import System.Process (system)

type Point = (Int, Int)

-- Rules of The Game
rules :: (Char, Int) -> Char
rules z
  | x == '.' && y == 3 = 'o' -- Reproduction
  | x == 'o' && y < 2 = '.' -- Underpopulation
  | x == 'o' && y > 3 = '.' -- Overcrowding
  | otherwise = x
  where
    x = fst z
    y = snd z

-- Map for 2D
map' :: (a -> b) -> [[a]] -> [[b]]
map' = map . map

-- Evolve Cells
evolveCell :: [[Char]] -> [[Int]] -> [[Char]]
evolveCell x y = map' rules (zipWith zip x y)

-- Gets Alive Cells
getAlive :: [[Char]] -> [Point]
getAlive a = [(x, y) | x <- [0 .. length a - 1], y <- [0 .. length (head a) - 1], a !! x !! y == 'o']

-- Converts Game to Points
convertPoint :: [[Char]] -> [[Point]]
convertPoint a = [[(x, y) | y <- [0 .. length (head a) - 1]] | x <- [0 .. length a - 1]]

-- Get All The Neighboring Points
getNeighborPoints :: Point -> [Point]
getNeighborPoints (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

-- Counts The Number of Neighboring Points That Are In The List of Points
countNeighbors :: Point -> [Point] -> Int
countNeighbors point points =
  let set1 = Set.fromList points
      set2 = Set.fromList $ getNeighborPoints point
      intersection = Set.intersection set1 set2
   in Set.size intersection

-- Converts Game Into Count of Neighbors Alive
getCountMap :: [[Char]] -> [[Int]]
getCountMap x = map' (`countNeighbors` getAlive x) (convertPoint x)

-- Calculates Next Generation
calculateNextGen :: String -> String
calculateNextGen x = unlines . evolveCell (lines x) $ getCountMap (lines x)

-- Game Loop
loop :: Int -> String -> IO ()
loop x y =
  when (x /= 0) $ do
    let nextGen = calculateNextGen y
    let delay = 100000
    threadDelay delay
    _ <- system "clear"
    putStrLn nextGen
    loop (x - 1) nextGen

-- Main Function
main :: IO ()
main = do
  game <- readFile "game.txt"
  let count = 250
  loop count game
