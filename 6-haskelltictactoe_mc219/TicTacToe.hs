module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver board
    = or (map ((\a -> case a of [Taken b] -> True; b -> False).(nub)) (rows board ++ cols board ++ diags board))
--    or (map ((\a -> a == [Taken O] || a == [Taken X]).(nub)) (rows board ++ cols board ++ diags board))
-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition str = intToPos (map (readMaybe) (words str))
    where
      intToPos :: [Maybe Int] -> Maybe Position
      intToPos [Just a, Just b] = Just (a,b)
      intToPos _                = Nothing

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (x,y) (cells,size)
  | x < 0 || y < 0 || x >= size || y >= size || index < 0 || index > size*size + 1 = Nothing
  | cells !! index == Empty            = board'
  | otherwise                          = Nothing


  where
    cells' = replace index (Taken player) cells
    board' = Just (cells', size)
    index  = x * size + y

-------------------------------------------------------------------
-- I/O Functions


prettyPrint :: Board -> IO ()
prettyPrint board@(cells, size)
  = putStrLn $ concat (concat (intersperse ["\n"] (map ((intersperse " ").(map (fromTaken))) (rows board) ) ) )
    where
      fromTaken :: Cell -> String
      fromTaken (Taken a) = show a
      fromTaken Empty     = "-"

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn board player
  = do
    putStr ("Player " ++ (show player) ++ ", make your move (row col): ")
    let try x = tryMove player (if isNothing (parsePosition x) then ((-1),(-1)) else fromJust (parsePosition x)) board
    newBoard <- doParseAction try :: IO Board

    return newBoard

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame board player
  = do
    prettyPrint board
    newBoard <- takeTurn board player
    if (gameOver newBoard)
      then do
        prettyPrint newBoard
        putStrLn ("Player " ++ (show player) ++ " has won!")
      else
        if (Empty `elem` (fst newBoard))
          then do
            let nextPlayer = if player == X then O else X
            playGame newBoard nextPlayer
          else do
            prettyPrint newBoard
            putStrLn "Its a draw!"

doParseAction :: (String -> Maybe a) -> IO a
doParseAction f
  = do
    line <- getLine
    let result = f line
    if isNothing result
      then do
        putStr "Invalid input, try again: "
        doParseAction f
      else
        return (fromJust result)



-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
    putStrLn "Welcome to tic tac toe on an N x N board"
    putStr "Enter the board size (N): "
    size <- doParseAction readSize :: IO Int
    let emptyBoard = replicate (size*size) Empty
    playGame (emptyBoard,size) X



    putStrLn "Thank you for playing"

readSize :: String -> Maybe Int
readSize x
  | isNothing (mayX)            = Nothing
  | fromJust (mayX) <= 0        = Nothing
  | otherwise                   = mayX
  where
    mayX = readMaybe x :: Maybe Int
-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
