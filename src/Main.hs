module Main where

import Graphics.Gloss

import Logic (transformGame)
import Rendering (gameAsPicture, background)
import Game (initialGame, tick)

windowSize :: (Int, Int)
windowSize = (330, 630)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = play display' background 60 initialGame gameAsPicture transformGame tick 
