module Rendering (gameAsPicture, background) where

import Graphics.Gloss (Color, Picture, translate, scale, pictures, greyN)
import Game (Game (tiles, tetromino), handleDrop)
import Tile (tilePicture, tilePicture')
import Tetromino (Tetromino(tiles'))

blockSize :: Float
blockSize = 30

background :: Color
background = greyN 0.1

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate (-5.5 * blockSize) (-10.5 * blockSize)
  . scale blockSize blockSize
  . pictures $ concat [board, shadow, tetro]
  where 
    shadow = map (tilePicture (greyN 0.15)) . tiles' . tetromino $ handleDrop game
    board  = map tilePicture' $ tiles game
    tetro  = map tilePicture' . tiles' $ tetromino game