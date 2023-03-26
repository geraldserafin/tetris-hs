module Rendering (gameAsPicture, background) where

import Graphics.Gloss 
import Game
import Tetromino
import Tile

blockSize :: Float
blockSize = 30

background :: Color
background = greyN 0.1

tilePicture' :: Tile ->  Picture
tilePicture' t = tilePicture (color' t) t

tilePicture :: Color -> Tile ->  Picture
tilePicture c (Tile _ (x,y)) = translate x y $ Color c (rectangleSolid 0.95 0.95)

holdPicture :: Maybe TetrominoKind -> Picture
holdPicture Nothing = blank
holdPicture (Just O) = translate (-0.5) 0 . pictures . map tilePicture' . tiles' $ getTetromino O
holdPicture (Just I) = translate (-1.5) (-0.5) . pictures . map tilePicture' . tiles' $ getTetromino I
holdPicture (Just t) = pictures . map tilePicture' . tiles' $ getTetromino t

gameAsPicture :: Game -> Picture
gameAsPicture game =
  scale blockSize blockSize . translate (-2.2) 0 $ pictures [boardPanel, boardPanel, holdPic, nextPic]
  where
    shadow = map (tilePicture (greyN 0.15)) . tiles' . tetromino $ handleDrop game
    placedTiles  = map tilePicture' $ tiles game
    tetro  = map tilePicture' . tiles' $ tetromino game
    board = translate (-5.5) (-10.5) . pictures $ concat [placedTiles, shadow, tetro]
    boardPanel = pictures [boardWire, board]
    boardWire = Color (greyN 0.20) $ rectangleWire 10.15 20.2
    h = holdPicture . fst $ hold game
    holdWire = translate 1 1.5 .  Color (greyN 0.20) $ rectangleWire 4.1 4.1
    holdPic = translate 6.4 6.5 $ pictures [h, holdWire]
    nextWire = translate 1 (-3) . Color (greyN 0.20) $ rectangleWire 4.1 13
    nextUp = pictures . zipWith (translate 0) [0, -3..] . map (holdPicture . Just) . take 4 $ next game
    nextPic = translate 6.4 (-0.6) $ pictures [nextUp, nextWire]
