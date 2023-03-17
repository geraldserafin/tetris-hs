module Main where

import Graphics.Gloss

import Events (transformGame)
import Game (initialGame, tick)
import Rendering (gameAsPicture, background)

windowSize :: (Int, Int)
windowSize = (310, 610)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = play display' background 60 initialGame gameAsPicture transformGame tick 
