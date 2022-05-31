import Control.Concurrent (threadDelay)
import Data.List (intersperse)

{-
Any live cell with fewer than two live neighbours dies, as if by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-}
-- \ESC
-- putStr "\ESC[2J"
main :: IO ()
main = do
  let board = [(4, 5), (4, 6), (5, 4), (4, 7)]
  let steps = iterate step board
  traverse printBoard $ take 5 steps
  pure ()

height :: Int
height = 10

width :: Int
width = 10

type Pos = (Int, Int)

type Board = [Pos]

step :: Board -> Board
step board =
  let potentiallyAliveCells = concatMap (\cell -> cell : neighbors cell) board
   in filter (isAliveInNext board) potentiallyAliveCells

printBoard :: Board -> IO ()
printBoard board = do
  putStr "\ESC[2J"
  putStrLn (board2str board)
  threadDelay 1000000

board2str :: Board -> String
{- board2str board = [ f (x,y) | x <- [1..width], y <- [1..height]] -}
board2str board =
  concat $
    intersperse "\n" $
      map
        ( \x ->
            map
              (\y -> f (x, y))
              [1 .. width]
        )
        [1 .. height]
  where
    f (a, b)
      | elem (a, b) board = '1'
      | otherwise = '0'

isAliveInNext :: Board -> Pos -> Bool
isAliveInNext b p
  | isCurrentlyAlive && (n == 3 || n == 2) = True
  | not isCurrentlyAlive && n == 3 = True
  | otherwise = False
  where
    isCurrentlyAlive = p `elem` b
    n = length $ filter (`elem` b) $ neighbors p

neighbors :: Pos -> [Pos] -- including itself??
neighbors (x, y) = [(rem a width, rem b height) | a <- [x -1 .. x + 1], b <- [y -1 .. y + 1], (a, b) /= (x, y)]
