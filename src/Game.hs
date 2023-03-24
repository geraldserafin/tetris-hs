module Game where
import Graphics.Gloss (Point)
import Control.Monad.Random (RandomGen)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import GHC.Float (int2Float)
import Tile (Tile (pos), RotationDirection, moveTile)

import Tetromino (Tetromino (angle, offsets, tiles'), down, rotateTetromino, invalidPosition, tests, moveTetromino, dropTetromino)
import Shuffle (infiniteTetrominos)

data Game = Game  {
  fallTime :: Float,
  tiles :: [Tile],
  tetromino :: Tetromino,
  tetrominos' :: [Tetromino]
}

handleDrop :: Game -> Game
handleDrop g@(Game _ ts t _) = g {
  fallTime = 1,
  tetromino = dropTetromino ts t
}

handleMove :: Point -> Game -> Game
handleMove md g@(Game _ ts t n)
  | invalidMove down     = clearRows g { tetromino = head n, tiles = ts ++ tiles' t, tetrominos' = tail n }
  | not (invalidMove md) = g { tetromino = moveTetromino md t }
  | otherwise = g
  where
    invalidMove x = invalidPosition ts $ moveTetromino x t

handleRotate :: RotationDirection -> Game -> Game
handleRotate rotationDir g@(Game _ ts tetro _) = g { tetromino = head (valid ++ [tetro]) }
  where
    valid = filter (not . invalidPosition ts)
          . map (rotateTetromino rotationDir . (`moveTetromino` tetro))
          $ tests rotationDir (angle tetro) (offsets tetro)

clearRows :: Game -> Game
clearRows g@(Game _ ts _ _) = g { tiles = concat moveDown }
  where
    rows = zip [0..] $ groupBy ((==) `on` (snd . pos)) $ sortBy (compare `on` (snd . pos)) ts :: [(Int, [Tile])]
    cleared = filter ((/=10) . length . snd) rows
    fullXs  = map fst $ filter ((==10) . length . snd) rows
    rowsBelow x = length $ filter (<x) fullXs
    moveDown = map (\(x1, t) -> map (moveTile (0, - int2Float (rowsBelow x1))) t) cleared

updateGame :: Float -> Game -> Game
updateGame et g@(Game ft _ _ _)
  | 1 <= ft   = (handleMove down g) { fallTime = 0 }
  | otherwise = g { fallTime = ft + et }

initialGame :: RandomGen g => g -> Game
initialGame gen = Game {
    fallTime   = 0,
    tiles      = [],
    tetromino   = head t,
    tetrominos' = tail t
  }
  where t = infiniteTetrominos gen
