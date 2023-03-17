module Game where

import Graphics.Gloss (Color, green)

data MoveDirection = MoveLeft | MoveRight | MoveDown
data Tile = Tile { color :: Color, pos :: (Float,Float) } deriving Eq
data Game = Game { tiles :: [Tile], fallT :: Float, tetronomicon :: [Tile] }

moveTile :: MoveDirection -> Tile -> Tile
moveTile MoveLeft  (Tile c (x, y)) = Tile c (x, y-1)
moveTile MoveRight (Tile c (x, y)) = Tile c (x, y+1)
moveTile MoveDown  (Tile c (x, y)) = Tile c (x+1, y)

moveTetronomicon :: MoveDirection -> Game -> Game
moveTetronomicon md game@(Game _ _ t) = game {
  tetronomicon = map (moveTile md) t
}

haveCommonElements :: Eq a => [a] -> [a] -> Bool
haveCommonElements xs ys = null ([ x | x <- xs , y <- ys, x == y])

overflows :: MoveDirection -> Game -> Bool
overflows MoveLeft  = (>=) 0  . minimum . map (snd . pos) . tetronomicon
overflows MoveRight = (<=) 9  . maximum . map (snd . pos) . tetronomicon
overflows MoveDown  = (<=) 19 . maximum . map (fst . pos) . tetronomicon

canMove :: MoveDirection -> Game -> Bool
canMove md game = haveCommonElements movedShape gameTiles && not (overflows md game)
  where
    movedShape = tetronomicon $ moveTetronomicon md game
    gameTiles  = tiles game

handleMove :: MoveDirection -> Game -> Game
handleMove md game = if canMove md game then moveTetronomicon md game else game

l :: [Tile]
l = map (Tile green) [(0,0), (0,1), (1,0), (2,0)]

initialGame :: Game
initialGame = Game {
  tiles = [],
  fallT = 0,
  tetronomicon = l
}

fall :: Game -> Game
fall g@(Game s _ t)
  | canMove MoveDown g = handleMove MoveDown g { fallT = 0 }
  | otherwise = g { fallT = 0, tetronomicon = l, tiles = s ++ t }

tick :: Float -> Game -> Game
tick elapsedTime game
  | ((>=1) . fallT) game = fall game
  | otherwise = game { fallT = fallT game + (elapsedTime * 16) }
