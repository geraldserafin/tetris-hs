module Events where
  
import Game
import Graphics.Gloss.Interface.IO.Game
import Tile
import qualified Data.Set as S

transformGame :: Event -> Game -> Game
transformGame (EventKey key state _ _) = handleKey key state 
transformGame _ = id

handleKey :: Key -> KeyState -> Game -> Game
handleKey (SpecialKey KeySpace) Down g = placeAndNext $ handleDrop g
handleKey (SpecialKey KeyUp   ) Down g = handleRotate RotateRight g
handleKey (SpecialKey key) Down g = handleSpecialKey key $ g { keys = S.insert (key, -0.15) (keys g) }
handleKey (SpecialKey key) Up   g = g { keys = S.filter ((/=key) . fst) (keys g) }
handleKey (Char key) Down       g = handleChar key g
handleKey _ _ g = g 

handleChar :: Char -> Game -> Game
handleChar 'z' = handleRotate RotateLeft 
handleChar 'x' = handleRotate RotateRight
handleChar 'c' = handleHold
handleChar _ = id
