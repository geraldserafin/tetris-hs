module Rendering (gameAsPicture, background) where

import Graphics.Gloss (Picture (Color), rectangleSolid, translate, scale, pictures, Color, greyN)
import Game (Tile (Tile, color'), Tetromino (tiles'), dropTetromino, Game (tiles, tetromino))

blockSize :: Float
blockSize = 30

background :: Color
background = greyN 0.1

tilePicture' :: Tile ->  Picture
tilePicture' t = tilePicture (color' t) t

tilePicture :: Color -> Tile ->  Picture
tilePicture c (Tile _ (x,y)) = translate x y $ Color c (rectangleSolid 1 1)

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate (-5.5 * blockSize) (-10.5 * blockSize)
  . scale blockSize blockSize
  . pictures $ concat [board, shadow, tetro]
  where 
    shadow = map (tilePicture (greyN 0.15)) . tiles' . tetromino $ dropTetromino game
    board  = map tilePicture' $ tiles game
    tetro  = map tilePicture' . tiles' $ tetromino game