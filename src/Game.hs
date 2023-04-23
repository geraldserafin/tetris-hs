module Game where
import Graphics.Gloss (Point)
import Control.Monad.Random (RandomGen)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import GHC.Float (int2Float)
import Tile (Tile (pos), RotationDirection (RotateRight), moveTile)
import qualified Data.Set as S

import Tetromino
import Graphics.Gloss.Interface.IO.Game (SpecialKey (KeyRight, KeyLeft, KeyDown, KeySpace, KeyUp))

data Game = Game  {
  fallTime :: Float,
  tiles :: [Tile],
  tetromino :: Tetromino,
  next :: [TetrominoKind],
  hold :: (Maybe TetrominoKind, Bool),
  keys :: S.Set (SpecialKey, Float)
}

handleDrop :: Game -> Game
handleDrop g = g {
  fallTime = 1,
  tetromino = dropTetromino (tiles g) (tetromino g)
}

handleHold :: Game -> Game
handleHold g@(Game _ _ _ _ (_, False) _) = g
handleHold g@(Game _ _ t n (Nothing, True) _) = g {
  tetromino = nextTetromino $ head n,
  next = tail n,
  hold = (Just (kind t), False)
}
handleHold g@(Game _ _ t _ (Just t', True) _) = g {
  tetromino = nextTetromino t',
  hold = (Just (kind t), False)
}

lock :: Game -> Game
lock g = clearRows g {
  tetromino = nextTetromino $ head (next g),
  tiles = tiles g ++ tiles' (tetromino g),
  next = tail (next g),
  hold = (fst $ hold g, True)
}

nextTetromino :: TetrominoKind -> Tetromino
nextTetromino = moveTetromino (4,21) . getTetromino

handleMove :: Point -> Game -> Game
handleMove md g
  | invalidMove down = lock g
  | not (invalidMove md) = g { tetromino = moveTetromino md (tetromino g) }
  | otherwise = g
  where
    invalidMove x = invalidPosition (tiles g)
                  $ moveTetromino x (tetromino g)

handleRotate :: RotationDirection -> Game -> Game
handleRotate rotationDir g = g { tetromino = head (valid ++ [tetro]) }
  where
    tetro = tetromino g
    valid = filter (not . invalidPosition (tiles g))
          . map (rotateTetromino rotationDir . (`moveTetromino` tetro))
          $ tests rotationDir (rotationState tetro) (kind tetro)

clearRows :: Game -> Game
clearRows g = g { tiles = concat moveDown }
  where
    rows = zip [0..] $ groupBy ((==) `on` (snd . pos)) $ sortBy (compare `on` (snd . pos)) (tiles g) :: [(Int, [Tile])]
    cleared = filter ((/=10) . length . snd) rows
    fullXs  = map fst $ filter ((==10) . length . snd) rows
    rowsBelow x = length $ filter (<x) fullXs
    moveDown = map (\(x1, t) -> map (moveTile (0, - int2Float (rowsBelow x1))) t) cleared

handleSpecialKey :: SpecialKey -> Game -> Game
handleSpecialKey KeyRight = handleMove right
handleSpecialKey KeyLeft  = handleMove left
handleSpecialKey KeyDown  = handleMove down
handleSpecialKey KeySpace = handleDrop
handleSpecialKey KeyUp    = handleRotate RotateRight
handleSpecialKey _  = id

handlePressedKeys :: Float -> Game -> Game
handlePressedKeys et g = (S.foldr f g (keys g)) { keys = S.map f' (keys g) }
  where
    f  (k, t) = if t >= 2*et then handleSpecialKey k else id
    f' (k, t) = if t >= 2*et then (k, 0) else (k, t+et*1.5)

gravitate :: Float -> Game -> Game
gravitate dt g
  | ft >= 1   = (handleMove down g) { fallTime = 0 }
  | otherwise = g { fallTime = ft + dt }
  where
    ft = fallTime g

updateGame :: Float -> Game -> Game
updateGame dt = handlePressedKeys dt . gravitate dt

initialGame :: RandomGen g => g -> Game
initialGame gen = do
   Game {
    fallTime  = 0,
    tiles     = [],
    tetromino = nextTetromino $ head t,
    next = tail t,
    hold = (Nothing, True),
    keys = S.empty
  }
  where t = infiniteTetrominos gen
