module Rendering (gameAsPicture, background) where

import Graphics.Gloss (Picture (Color), rectangleSolid, translate, scale, pictures, Color, greyN)
import GHC.Float (int2Float)
import Game (Game (tetrominoe, blocks), Block (Block))
import Data.Maybe (catMaybes)

blockSize :: Float
blockSize = 30

background :: Color
background = greyN 0.2

blockPicture :: Block -> Picture
blockPicture (Block c (x,y)) = translate (int2Float y+1) (int2Float $ -x-1) $ Color c (rectangleSolid 1 1)

gameAsPicture :: Game -> Picture
gameAsPicture game =
  translate (-5 * blockSize - blockSize/2) (10 * blockSize + blockSize/2)
  . scale blockSize blockSize
  . pictures
  . map blockPicture
  . (++) (catMaybes . concat $ tetrominoe game)
  $ blocks game