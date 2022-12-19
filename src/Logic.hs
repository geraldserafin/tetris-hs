module Logic where

import Game (Game)
import Graphics.Gloss.Interface.IO.Game (Event)

transformGame :: Event -> Game -> Game
transformGame _ game = game