{-# LANGUAGE InstanceSigs #-}
module Game where

import qualified Graphics.Gloss.Data.Point.Arithmetic as P ((+), (-))
import Graphics.Gloss (Color, orange, red, blue, Point, yellow, violet, aquamarine, azure)
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Random (MonadRandom(getRandomRs))

data Tile = Tile { color' :: Color, pos :: Point}

instance Eq Tile where
  (==) :: Tile -> Tile -> Bool
  (==) (Tile _ p1) (Tile _ p2) = p1 == p2

data Tetromino = Tetromino {
  middle :: Point,
  rotation :: Rotation,
  offsets  :: Rotation -> [Point],
  tiles' :: [Tile]
}

data Game = Game  { 
  fallTime :: Float,  
  tiles :: [Tile], 
  tetromino :: Tetromino, 
  tetrominos' :: [Tetromino] 
}

data RotationDirection = RotateLeft | RotateRight
data Rotation = L | O | R | Z deriving Enum

left, right, down :: Point
left  = (-1, 0)
right = ( 1, 0)
down  = ( 0,-1)

moveTile :: Point -> Tile -> Tile
moveTile p1 (Tile c p2) = Tile c (p1 P.+ p2)

rotateTile :: RotationDirection -> Point -> Tile -> Tile
rotateTile RotateRight (cx, cy) (Tile e (x,y)) = Tile e (y-cy+cx , negate (x-cx)+cy)
rotateTile RotateLeft  (cx, cy) (Tile e (x,y)) = Tile e (negate (y-cy)+cx, x-cx+cy)

moveTetromino :: Point -> Tetromino -> Tetromino
moveTetromino p1 t = t {
  middle = middle t P.+ p1,
  tiles' = map (moveTile p1) (tiles' t)
}

rotateTetrominoe :: RotationDirection -> Tetromino -> Tetromino
rotateTetrominoe rd t = t {
  rotation = nextRotation rd (rotation t),
  tiles'   = map (rotateTile rd $ middle t) (tiles' t)
}

overflows :: Tetromino -> Bool
overflows = any (liftA2 (||) xs ys . pos) . tiles'
  where
    xs = (`notElem` [1..10]) . fst
    ys = (`notElem` [1..24]) . snd

collides :: [Tile] -> Tetromino -> Bool
collides b = any (`elem` b) . tiles'

handleMove :: Point -> Game -> Game
handleMove md g@(Game _ ts t n)
  | invalidMove down     = g { tetromino = head n, tiles = ts ++ tiles' t, tetrominos' = tail n }
  | not (invalidMove md) = g { tetromino = moveTetromino md t }
  | otherwise = g
  where
    invalidMove x = liftA2 (||) overflows (collides ts) $ moveTetromino x t

dropTetromino :: Game -> Game
dropTetromino g@(Game _ ts t _) = g {
  tetromino = moveTetromino (0, 1) . until (liftA2 (||) overflows (collides ts)) (moveTetromino down) $ t
} 

handleRotate :: RotationDirection -> Game -> Game
handleRotate rd g@(Game _ b t _) = g { tetromino = head (valid ++ [t]) }
  where
    kick  = not . liftA2 (||) overflows (collides b)
    valid = filter kick
          . map (rotateTetrominoe rd . (`moveTetromino` t))
          $ getTests rd (rotation t) (offsets t)

getTests :: RotationDirection -> Rotation -> (Rotation -> [Point]) -> [Point]
getTests rd r o = zipWith (P.-) (o r) (o $ nextRotation rd r)

nextRotation :: RotationDirection -> Rotation -> Rotation
nextRotation RotateLeft  L = Z
nextRotation RotateRight Z = L
nextRotation RotateLeft  x = pred x
nextRotation RotateRight x = succ x

randomTetrominos :: (MonadRandom m) => m [Tetromino]
randomTetrominos = do
  xs <- getRandomRs (0, length tetrominos - 1)
  return $ map (tetrominos !!) xs

tetrominos :: [Tetromino]
tetrominos = map (moveTetromino (5,20)) [j,l,s,t',z,i,o']

initialGame :: MonadRandom m => m Game
initialGame = do
  r <- randomTetrominos
  return Game {
    fallTime   = 0,
    tiles      = [],
    tetromino   = head r,
    tetrominos' = tail r
  }


tick :: Float -> Game -> Game
tick et g@(Game ft _ _ _) 
  | 1 <= ft   = (handleMove down g) { fallTime = 0 }
  | otherwise = g { fallTime = ft + et * 4 }

--- terominos definitions

jlstzOffsets, iOffsets, oOffsets :: Rotation -> [Point]
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
j = jlstzBase (map (Tile blue  ) [(0,2), (0,1), (1,1), (2,1)])
l = jlstzBase (map (Tile orange) [(0,1), (1,1), (2,1), (2,2)])
t'= jlstzBase (map (Tile violet) [(0,1), (1,1), (1,2), (2,1)])
z = jlstzBase (map (Tile red   ) [(0,2), (1,2), (1,1), (2,1)])
i = Tetromino {
  middle   = (2,2),
  rotation = O,
  offsets  = iOffsets,
  tiles' = map (Tile azure) [(1,2), (2,2), (3,2), (4,2)]
}
o' = Tetromino {
  middle   = (1,1),
  rotation = O,
  offsets  = oOffsets,
  tiles' = map (Tile yellow) [(1,1), (1,2), (2,1), (2,2)]
}
