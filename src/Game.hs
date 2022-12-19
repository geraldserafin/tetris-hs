module Game where

import Graphics.Gloss (Color)
import Data.Matrix (Matrix (nrows), matrix)

type Board = Matrix Tile 

data Tile = Empty | Placed Color deriving Show
data Game = Game { 
  board :: Board, 
  score :: Int, 
  pos   :: (Int, Int), 
  fallT :: Float, 
  level :: Float 
}

initialGame :: Game
initialGame = Game { 
  board = matrix 20 10 (const Empty), 
  score = 0, 
  fallT = 0,
  pos   = (1,1),
  level = 5
}

tick :: Float -> Game -> Game
tick elapsedTime game 
  | x < nrows b && ft >= 1 = game { pos = (x+1, y), fallT = 0 } 
  | otherwise              = game { fallT = ft + (elapsedTime * level game) }
  where
    b      = board game
    ft     = fallT game
    (x, y) = pos game
