module Events where

import Graphics.Gloss.Interface.IO.Game (Event(EventKey), SpecialKey (KeyRight, KeyLeft), Key (SpecialKey), KeyState (Down))
import Game (Game, handleMove, MoveDirection (MoveRight, MoveLeft))

transformGame :: Event -> Game -> Game
transformGame (EventKey key state _ _) = handleKey key state 
transformGame _  = id

handleKey :: Key -> KeyState -> Game -> Game
handleKey (SpecialKey key) Down game = handleSpecialKey key game
handleKey _ _ game  = game 

handleSpecialKey :: SpecialKey -> Game -> Game
handleSpecialKey KeyRight = handleMove MoveRight
handleSpecialKey KeyLeft  = handleMove MoveLeft 
handleSpecialKey _  = id
