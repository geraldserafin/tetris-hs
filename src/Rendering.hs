module Rendering where

import Graphics.Gloss (Picture (Color), Color, makeColor, rectangleSolid, pictures, translate, red)
import Game (Game (board, pos), Tile (Empty, Placed))
import Data.Matrix (toList, mapPos, setElem)
import GHC.Float (int2Float)

background :: Color
background = makeColor 0 0 0 255

tileSize :: Float
tileSize = 30

tilePicture :: Tile -> Picture
tilePicture  Empty     = rectangleSolid tileSize tileSize
tilePicture (Placed c) = Color c (rectangleSolid tileSize tileSize)

translateTile :: (Int, Int) -> Picture -> Picture
translateTile (r, c) = translate (-135 + int2Float (c-1) * tileSize) (285 - int2Float (r-1) * tileSize)

renderTile :: (Int, Int) -> Tile -> Picture
renderTile    (r, c) = translateTile (r, c) . tilePicture 

gameAsPicture :: Game -> Picture
gameAsPicture game = 
  pictures 
  . reverse 
  . toList 
  . mapPos renderTile 
  . setElem (Placed red) (pos game) 
  $ board game