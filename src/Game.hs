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
  keys :: S.Set (SpecialKey, Float),
  ground :: (Bool, Float),
  level :: (Int, Int)
}

handleDrop :: Game -> Game
handleDrop g = g {
  fallTime = 1,
  tetromino = dropTetromino (tiles g) (tetromino g)
}

handleHold :: Game -> Game
handleHold g@(Game _ _ _ _ (_, False) _ _ _) = g
handleHold g@(Game _ _ t n (Nothing, True) _ _ _) = g {
  tetromino = nextTetromino $ head n,
  next = tail n,
  hold = (Just (kind t), False)
}
handleHold g@(Game _ _ t _ (Just t', True) _ _ _) = g {
  tetromino = nextTetromino t',
  hold = (Just (kind t), False)
}

placeAndNext :: Game -> Game
placeAndNext g = clearRows g {
  tetromino = nextTetromino $ head (next g),
  tiles = tiles g ++ tiles' (tetromino g),
  next = tail (next g),
  hold = (fst $ hold g, True)
}

nextTetromino :: TetrominoKind -> Tetromino
nextTetromino = moveTetromino (4,21) . getTetromino

handleMove :: Point -> Game -> Game
handleMove md g
  | invalidMove down && not (fst $ ground g) = g { ground = (True, 0) }
  | not (invalidMove md) = g { tetromino = moveTetromino md (tetromino g), ground = if moved then (False, 0) else ground g }
  | otherwise = g
  where
    moved = not . and $ zipWith (==) (tiles' $ tetromino g) (tiles' . moveTetromino md $ tetromino g)
    invalidMove x = invalidPosition (tiles g)
                  $ moveTetromino x (tetromino g)

handleRotate :: RotationDirection -> Game -> Game
handleRotate rotationDir g = g { tetromino = head (valid ++ [tetro]), ground = if null valid then ground g else (fst $ ground g, 0)}
  where
    tetro = tetromino g
    valid = filter (not . invalidPosition (tiles g))
          . map (rotateTetromino rotationDir . (`moveTetromino` tetro))
          $ tests rotationDir (rotationState tetro) (kind tetro)

clearRows :: Game -> Game
clearRows g = g { tiles = concat moveDown, level = l' }
  where
    (l, c) = level g 
    c' = c + length fullXs
    l' = if c' >= 2 then (l+1, c'-5) else (l, c')
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

handleGround :: Float -> Game -> Game
handleGround et g
  | gt >=0.5 && invalidMove down = placeAndNext g { ground = (False, 0) }
  | g' = g { ground = (g', gt + et)  }
  | otherwise = g
  where
    (g', gt) = ground g
    invalidMove x = invalidPosition (tiles g)
                  $ moveTetromino x (tetromino g)

updateGame :: Float -> Game -> Game
updateGame dt g
  | ft >= 1   = handleGround dt . handlePressedKeys dt $ (handleMove down g) { fallTime = 0 }
  | otherwise = handleGround dt . handlePressedKeys dt $ g { fallTime = ft + dt * lvl }
  where
    ft = fallTime g
    lvl = int2Float . fst $ level g

initialGame :: RandomGen g => g -> Game
initialGame gen = do
   Game {
    fallTime  = 0,
    tiles     = [],
    tetromino = nextTetromino $ head t,
    next = tail t,
    hold = (Nothing, True),
    keys = S.empty,
    ground = (False, 0),
    level = (1, 0)
  }
  where t = infiniteTetrominos gen
