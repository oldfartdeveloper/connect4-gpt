module Connect4 (main) where

import Data.List (elemIndex, intercalate, subsequences, transpose)

type Board = [[Cell]]
type Cell = Maybe Player
data Player = Red | Yellow deriving (Eq, Show)

main :: IO ()
main = gameLoop initialBoard Red

gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
  printBoard board
  putStrLn $ show player ++ "'s turn."
  putStrLn "Choose a column (0-6):"
  colStr <- getLine
  case readMaybe colStr of
    Nothing -> do
      putStrLn "Invalid input. Please enter a number from 0 to 6."
      gameLoop board player
    Just col -> do
      case dropDisc player col board of
        Nothing -> do
          putStrLn "Invalid move. Please choose another column."
          gameLoop board player
        Just newBoard ->
          if isWin player newBoard then do
            putStrLn $ show player ++ " wins!"
            printBoard newBoard
          else
            gameLoop newBoard (otherPlayer player)

otherPlayer :: Player -> Player
otherPlayer Red = Yellow
otherPlayer Yellow = Red

printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines (map printRow (transpose board)))
  where
    printRow row = intercalate " | " (map printCell row)
    printCell Nothing = " "
    printCell (Just Red) = "R"
    printCell (Just Yellow) = "Y"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing


emptyBoard :: Int -> Int -> Board
emptyBoard numRows numCols = replicate numCols (replicate numRows Nothing)

initialBoard :: Board
initialBoard = emptyBoard 6 7

dropDisc :: Player -> Int -> Board -> Maybe Board
dropDisc player col board
  | col < 0 || col >= length board = Nothing
  | otherwise =
      case dropInColumn player (board !! col) of
        Nothing -> Nothing
        Just newCol -> Just (replace col newCol board)

dropInColumn :: Player -> [Cell] -> Maybe [Cell]
dropInColumn player column = case firstEmptySlot column of
  Nothing -> Nothing
  Just i -> Just (replace i (Just player) column)

firstEmptySlot :: [Cell] -> Maybe Int
firstEmptySlot = elemIndex Nothing

replace :: Int -> a -> [a] -> [a]
replace i x xs = take i xs ++ [x] ++ drop (i + 1) xs

isWin :: Player -> Board -> Bool
isWin player board =
  let rows = transpose board
      diags1 = diagonals board
      diags2 = diagonals (reverse board)
      allLines = board ++ rows ++ diags1 ++ diags2
  in any (isConsecutive 4 $ Just player) allLines

isConsecutive :: Eq a => Int -> a -> [a] -> Bool
isConsecutive n x xs = any (all (== x)) (subsequencesOfSize n xs)

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = filter (\subseq -> length subseq == n) (subsequences xs)

diagonals :: Board -> [[Cell]]
diagonals board = [diag i | i <- [-(numRows - 1)..numCols - 1]]
  where
    numRows = length (head board)
    numCols = length board
    diag i = [board !! (j + i) !! j | j <- [0..min (numCols - i - 1) (numRows - 1)], i + j >= 0]