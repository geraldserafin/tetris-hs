module Rendering (gameAsPicture, background) where

import Game (Game (tetronomicon, tiles), Tile (Tile))
import Graphics.Gloss (Picture (Color), Color, makeColor, rectangleSolid, translate, scale, pictures)

background :: Color
background = makeColor 255 255 255 255

tileSize :: Float
tileSize = 30

tilePicture :: Tile -> Picture
tilePicture (Tile c (x, y)) = translate y (-x) $ Color c (rectangleSolid 1 1)

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate (-5 * tileSize + tileSize / 2) (10 * tileSize - tileSize / 2)
  . scale tileSize tileSize
  . pictures
  . map tilePicture
  $ tiles game ++ tetronomicon game