module Tetromino where

import Graphics.Gloss (Point, aquamarine, blue, orange, violet, red, azure, yellow)
import Tile (Angle (Z, L, R, O), Tile (pos, Tile), RotationDirection (RotateLeft, RotateRight), moveTile, rotateTile)
import qualified Graphics.Gloss.Data.Point.Arithmetic as P ((+), (-))
import Control.Applicative (Applicative(liftA2))

data Tetromino = Tetromino {
  center  :: Point,
  angle   :: Angle,
  offsets :: Angle -> [Point],
  tiles'  :: [Tile]
}

left, right, down :: Point
left  = (-1, 0)
right = ( 1, 0)
down  = ( 0,-1)

nextAngle :: RotationDirection -> Angle -> Angle
nextAngle RotateLeft  L = Z
nextAngle RotateRight Z = L
nextAngle RotateLeft  x = pred x
nextAngle RotateRight x = succ x

moveTetromino :: Point -> Tetromino -> Tetromino
moveTetromino p1 t = t {
  center = center t P.+ p1,
  tiles' = map (moveTile p1) (tiles' t)
}

rotateTetromino :: RotationDirection -> Tetromino -> Tetromino
rotateTetromino rd t = t {
  angle  = nextAngle rd (angle t),
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

tests :: RotationDirection -> Angle -> (Angle -> [Point]) -> [Point]
tests rd r o = zipWith (P.-) (o r) (o $ nextAngle rd r)

--- terominos definitions

tetrominos :: [Tetromino]
tetrominos = map (moveTetromino (5,20)) [j,l,s,t',z,i,o']

jlstzOffsets, iOffsets, oOffsets :: Angle -> [Point]
jlstzOffsets O = [(0,0), ( 0,0), ( 0, 0), (0,0), ( 0,0)]
jlstzOffsets R = [(0,0), ( 1,0), ( 1,-1), (0,2), ( 1,2)]
jlstzOffsets Z = [(0,0), ( 0,0), ( 0, 0), (0,0), ( 0,0)]
jlstzOffsets L = [(0,0), (-1,0), (-1,-1), (0,2), (-1,2)]

iOffsets O = [( 0,0), (-1,0), ( 2,0), (-1, 0), ( 2,0)]
iOffsets R = [(-1,0), ( 0,0), ( 0,0), ( 0, 1), ( 0,2)]
iOffsets Z = [(-1,1), ( 1,1), (-2,1), ( 1, 0), (-2,0)]
iOffsets L = [( 0,1), ( 0,1), ( 0,1), ( 0,-1), ( 0,2)]

oOffsets O = [( 0, 0)]
oOffsets R = [( 0,-1)]
oOffsets Z = [(-1,-1)]
oOffsets L = [(-1, 0)]

jlstzBase :: [Tile] -> Tetromino
jlstzBase = Tetromino (1,1) O jlstzOffsets

j, l, s, t', z, i, o' :: Tetromino
s = jlstzBase (map (Tile aquamarine) [(0,1), (1,1), (1,2), (2,2)])
j = jlstzBase (map (Tile blue  )     [(0,2), (0,1), (1,1), (2,1)])
l = jlstzBase (map (Tile orange)     [(0,1), (1,1), (2,1), (2,2)])
t'= jlstzBase (map (Tile violet)     [(0,1), (1,1), (1,2), (2,1)])
z = jlstzBase (map (Tile red   )     [(0,2), (1,2), (1,1), (2,1)])
i = Tetromino {
  center  = (2,2),
  angle   = O,
  offsets = iOffsets,
  tiles'  = map (Tile azure) [(1,2), (2,2), (3,2), (4,2)]
}
o' = Tetromino {
  center  = (1,1),
  angle   = O,
  offsets = oOffsets,
  tiles'  = map (Tile yellow) [(1,1), (1,2), (2,1), (2,2)]
}
