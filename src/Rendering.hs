module Rendering (gameAsPicture, background) where

import Graphics.Gloss (Picture (Color), rectangleSolid, translate, scale, pictures, Color, greyN)
import Game (Game (tiles, tetromino, Game), Tile (Tile), Tetromino (tiles'), overflows, collides, moveTetromino, down)
import Control.Applicative (Applicative(liftA2))

blockSize :: Float
blockSize = 30

background :: Color
background = greyN 0.1

tilePicture :: Tile ->  Picture
tilePicture (Tile c (x,y)) = translate x y $ Color c (rectangleSolid 1 1)

tilePicture' :: Color -> Tile ->  Picture
tilePicture' c (Tile _ (x,y)) = translate x y $ Color c (rectangleSolid 1 1)

a :: Game -> [Picture]
a (Game _ ts t _) = map (tilePicture' (greyN 0.15)) . tiles' . moveTetromino (0, 1) . until (liftA2 (||) overflows (collides ts)) (moveTetromino down) $ t

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate (-5.5 * blockSize) (-10.5 * blockSize)
  . scale blockSize blockSize
  . pictures 
  . (++) (a game)
  . map tilePicture
  . liftA2 (++) (tiles' . tetromino) tiles $ game