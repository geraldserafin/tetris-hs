module Main where

import Graphics.Gloss
import Events (transformGame)
import Game (initialGame, updateGame)
import Rendering (gameAsPicture, background)
import Control.Monad.Random (getStdGen)

windowSize :: (Int, Int)
windowSize = (330, 630)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = do 
  gen <- getStdGen
  play display' background 120 (initialGame gen) gameAsPicture transformGame updateGame 
