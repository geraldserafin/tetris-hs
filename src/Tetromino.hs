{-# OPTIONS_GHC -Wno-deprecations #-}
module Tetromino where

import Graphics.Gloss (Point, aquamarine, blue, orange, violet, red, azure, yellow, makeColor, makeColorI, cyan, green)
import Tile (RotationState (R2, RL, RR, R0), Tile (pos, Tile), RotationDirection (RotateLeft, RotateRight), moveTile, rotateTile)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P ((+), (-))
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Random (RandomGen (next))
import System.Random.Shuffle (shuffle')

data TetrominoKind = I | J | L | O | S | T | Z

data Tetromino = Tetromino {
  center  :: Point,
  rotationState :: RotationState,
  kind :: TetrominoKind,
  tiles' :: [Tile]
}

left, right, down :: Point
left  = (-1, 0)
right = ( 1, 0)
down  = ( 0,-1)

nextRotationState :: RotationDirection -> RotationState -> RotationState
nextRotationState RotateLeft  RL = R2
nextRotationState RotateRight R2 = RL
nextRotationState RotateLeft  x = pred x
nextRotationState RotateRight x = succ x

moveTetromino :: Point -> Tetromino -> Tetromino
moveTetromino p1 t = t {
  center = center t P.+ p1,
  tiles' = map (moveTile p1) (tiles' t)
}

moveTetrominoTo :: Point -> Tetromino -> Tetromino
moveTetrominoTo p1 t = moveTetromino (p1 P.- center t) t

rotateTetromino :: RotationDirection -> Tetromino -> Tetromino
rotateTetromino rd t = t {
  rotationState = nextRotationState rd (rotationState t),
  tiles' = map (rotateTile rd $ center t) (tiles' t)
}

dropTetromino :: [Tile] -> Tetromino -> Tetromino
dropTetromino ts = until (liftA2 (||) overflows (occupied ts) . moveTetromino down) (moveTetromino down)

overflows :: Tetromino -> Bool
overflows = any (liftA2 (||) xs ys . pos) . tiles'
  where
    xs = (`notElem` [1..10]) . fst
    ys = (`notElem` [1..24]) . snd

occupied :: [Tile] -> Tetromino -> Bool
occupied b = any (`elem` b) . tiles'

invalidPosition :: [Tile] -> Tetromino -> Bool
invalidPosition ts = liftA2 (||) overflows (occupied ts)

tests :: RotationDirection -> RotationState -> TetrominoKind -> [Point]
tests rd rs k = zipWith (P.-) (offset k rs) (offset k $ nextRotationState rd rs)

--- terominos definitions

tetrominos :: [TetrominoKind]
tetrominos = [S, J, L, T, Z, O, I]

infiniteTetrominos :: RandomGen g => g -> [TetrominoKind]
infiniteTetrominos g = do
  let shuffled = shuffle' tetrominos 7 g
  let (_, g')  = next g
  shuffled ++ infiniteTetrominos g'

offset :: TetrominoKind -> RotationState -> [Point]
offset I R0 = [( 0,0), (-1,0), ( 2,0), (-1, 0), ( 2,0)]
offset I RR = [(-1,0), ( 0,0), ( 0,0), ( 0, 1), ( 0,2)]
offset I R2 = [(-1,1), ( 1,1), (-2,1), ( 1, 0), (-2,0)]
offset I RL = [( 0,1), ( 0,1), ( 0,1), ( 0,-1), ( 0,2)]

offset O R0 = [( 0, 0)]
offset O RR = [( 0,-1)]
offset O R2 = [(-1,-1)]
offset O RL = [(-1, 0)]

offset _ R0 = [(0,0), ( 0,0), ( 0, 0), (0,0), ( 0,0)]
offset _ RR = [(0,0), ( 1,0), ( 1,-1), (0,2), ( 1,2)]
offset _ R2 = [(0,0), ( 0,0), ( 0, 0), (0,0), ( 0,0)]
offset _ RL = [(0,0), (-1,0), (-1,-1), (0,2), (-1,2)]

tetrominoBase :: TetrominoKind -> [Tile] -> Tetromino
tetrominoBase = Tetromino (1,1) R0


getTetromino :: TetrominoKind -> Tetromino
getTetromino S = tetrominoBase S (map (Tile green) [(0,1), (1,1), (1,2), (2,2)])
getTetromino J = tetrominoBase J (map (Tile blue) [(0,2), (0,1), (1,1), (2,1)])
getTetromino L = tetrominoBase L (map (Tile orange)     [(0,1), (1,1), (2,1), (2,2)])
getTetromino T = tetrominoBase T (map (Tile violet)[(0,1), (1,1), (1,2), (2,1)])
getTetromino Z = tetrominoBase Z (map (Tile red   )     [(0,2), (1,2), (1,1), (2,1)])
getTetromino O = tetrominoBase O (map (Tile yellow)     [(1,1), (1,2), (2,1), (2,2)])
getTetromino I = Tetromino {
  center  = (2,2),
  rotationState = R0,
  kind = I,
  tiles' = map (Tile cyan) [(1,2), (2,2), (3,2), (4,2)]
}
