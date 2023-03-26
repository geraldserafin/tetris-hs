module Main where

import Graphics.Gloss (play, Display(InWindow))
import Events 
import Game 
import Rendering 
import Control.Monad.Random

-- width:  10 x 30 + 20
-- heigth: 20 x 30 + 20

windowSize :: (Int, Int)
windowSize = (450, 620)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = do 
  gen <- getStdGen
  play display' background 60 (initialGame gen) gameAsPicture transformGame updateGame 
