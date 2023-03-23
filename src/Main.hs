module Main where

import Graphics.Gloss
import Events (transformGame)
import Game (initialGame, tick)
import Rendering (gameAsPicture, background)

windowSize :: (Int, Int)
windowSize = (330, 630)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = do 
  initialGame' <- initialGame
  play display' background 120 initialGame' gameAsPicture transformGame tick 
