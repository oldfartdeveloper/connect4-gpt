module Connect4
  ( Player (..)
  , Board
  , newBoard
  , displayBoard
  , playMove
  , checkWinner
  ) where

data Player = Red | Yellow deriving (Show, Eq)

type Board = [[Maybe Player]]

newBoard :: Int -> Int -> Board
newBoard rows cols = replicate rows (replicate cols Nothing)

displayBoard :: Board -> String
displayBoard = unlines . map (concatMap cellToString)
  where
    cellToString Nothing = ". "
    cellToString (Just Red) = "R "
    cellToString (Just Yellow) = "Y "

playMove :: Board -> Int -> Player -> Either String Board
playMove board col player = undefined

checkWinner :: Board -> Maybe Player
checkWinner board = undefined