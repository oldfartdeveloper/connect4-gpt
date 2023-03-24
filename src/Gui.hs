module Gui (entry) where

import GHC.Float.RealFracMethods (float2Int, int2Float)
import Data.Maybe (isNothing)
import Connect4
    ( initialBoard, Board, Cell, Column, Player(..), otherPlayer )

import Graphics.Gloss
    ( greyN,
      red,
      white,
      yellow,
      circleSolid,
      color,
      pictures,
      translate,
      play,
      Display(InWindow),
      Picture )
import Graphics.Gloss.Interface.Pure.Game
    ( Key(Char, MouseButton),
      Event(EventKey),
      KeyState(Down),
      MouseButton(LeftButton),
      scale,
      text )

data Game = Game
  { gameBoard :: Board
  , currentPlayer :: Player
  , history :: [Board]
  , future :: [Board]
  }

entry :: IO ()
entry = play
  (InWindow "Connect 4" (screenWidth, screenHeight) (0, 0))
  white
  30
  initialGame
  drawGame
  handleEvent
  updateGame

initialGame :: Game
initialGame = Game initialBoard Red [] []

screenWidth, screenHeight :: Int
screenWidth = 700
screenHeight = 600

drawGame :: Game -> Picture
drawGame game = pictures
  [ drawBoard (gameBoard game)
  , drawOverlay (currentPlayer game)
  ]

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, _)) game =
  let
      col = (float2Int x + screenWidth `div` 2) `div` cellSize
  in
    case dropDisc (currentPlayer game) col (gameBoard game) of
    Nothing -> game
    Just newBoard ->
      let newPlayer = otherPlayer (currentPlayer game)
          newHistory = gameBoard game : history game
      in game { gameBoard = newBoard, currentPlayer = newPlayer, history = newHistory, future = [] }

handleEvent (EventKey (Char 'z') Down _ _) game@Game { history = (prevBoard : prevHistory) } =
  game { gameBoard = prevBoard, currentPlayer = otherPlayer (currentPlayer game), history = prevHistory, future = gameBoard game : future game }

handleEvent (EventKey (Char 'y') Down _ _) game@Game { future = (nextBoard : nextFuture) } =
  game { gameBoard = nextBoard, currentPlayer = otherPlayer (currentPlayer game), history = gameBoard game : history game, future = nextFuture }

handleEvent _ game = game

dropDisc :: Player -> Int -> Board -> Maybe Board
dropDisc player col board
  | col < 0 || col >= length board = Nothing
  | otherwise = dropInColumn player col board
    where
      dropInColumn :: Player -> Int -> Board -> Maybe Board
      dropInColumn player' col' board' = do
        let column = board' !! col'
        newRow <- findEmptyRow column
        let newColumn = replaceNth newRow (Just player') column
        return (replaceNth col newColumn board')

      findEmptyRow :: Column -> Maybe Int
      findEmptyRow column = case filter (\(_, cell) -> isNothing cell) (zip [0..] column) of
        [] -> Nothing
        xs -> Just . fst $ last xs

      replaceNth :: Int -> a -> [a] -> [a]
      replaceNth n newVal (x:xs)
        | n == 0 = newVal : xs
        | otherwise = x : replaceNth (n - 1) newVal xs
      replaceNth _ _ [] = []

-- No updates needed
updateGame :: Float -> Game -> Game
updateGame _ = id

drawBoard :: Board -> Picture
drawBoard board = pictures [drawCell x y cell | (x, col) <- zip [0..] board, (y, cell) <- zip [0..] col]

drawCell :: Int -> Int -> Cell -> Picture
drawCell x y cell = translate (fromIntegral (x * cellSize' - halfWidth)) (fromIntegral (y * cellSize' - halfHeight)) (color cellColor (circleSolid (int2Float cellSize' * 0.45)))
  where
    cellSize' = cellSize
    halfWidth = screenWidth `div` 2
    halfHeight = screenHeight `div` 2
    cellColor = case cell of
      Nothing -> greyN 0.8
      Just Red -> red
      Just Yellow -> yellow

cellSize :: Int
cellSize =
  screenWidth `div` length initialBoard

drawOverlay :: Player -> Picture
drawOverlay player = translate (-fromIntegral halfWidth * 0.8) (fromIntegral halfHeight * 0.8) (scale 0.25 0.25 (text (show player ++ "'s turn")))
  where
    halfWidth = screenWidth `div` 2
    halfHeight = screenHeight `div` 2
