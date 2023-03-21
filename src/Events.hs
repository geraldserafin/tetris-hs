module Events where
import Game (Game, handleMove, Move (MoveLeft, MoveDown, MoveRight))
import Graphics.Gloss.Interface.IO.Game (KeyState (Down), Key (SpecialKey), SpecialKey (KeyRight, KeyLeft, KeyDown), Event (EventKey))


transformGame :: Event -> Game -> Game
transformGame (EventKey key state _ _) = handleKey key state 
transformGame _ = id

handleKey :: Key -> KeyState -> Game -> Game
handleKey (SpecialKey key) Down = handleSpecialKey key
handleKey _ _  = id 

handleSpecialKey :: SpecialKey -> Game -> Game
handleSpecialKey KeyRight = handleMove MoveRight
handleSpecialKey KeyLeft  = handleMove MoveLeft 
handleSpecialKey KeyDown  = handleMove MoveDown 
handleSpecialKey _  = id
