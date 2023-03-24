module Events where
import Game (Game, handleMove, handleRotate, handleDrop)
import Graphics.Gloss.Interface.IO.Game (KeyState (Down), Key (SpecialKey, Char), SpecialKey (KeyRight, KeyLeft, KeyDown, KeyUp, KeySpace), Event (EventKey))
import Tile (RotationDirection(RotateLeft, RotateRight))
import Tetromino (right, left, down)

transformGame :: Event -> Game -> Game
transformGame (EventKey key state _ _) = handleKey key state 
transformGame _ = id

handleKey :: Key -> KeyState -> Game -> Game
handleKey (SpecialKey key) Down = handleSpecialKey key
handleKey (Char key) Down       = handleChar key
handleKey _ _  = id 

handleChar :: Char -> Game -> Game
handleChar 'z' = handleRotate RotateLeft 
handleChar 'x' = handleRotate RotateRight
handleChar _ = id

handleSpecialKey :: SpecialKey -> Game -> Game
handleSpecialKey KeyRight = handleMove right
handleSpecialKey KeyLeft  = handleMove left 
handleSpecialKey KeyDown  = handleMove down 
handleSpecialKey KeySpace = handleDrop
handleSpecialKey KeyUp    = handleRotate RotateRight
handleSpecialKey _  = id
