module Main where

import Graphics.Gloss

import Events (transformGame)
import Game (initialGame, tick)
import Rendering (gameAsPicture, background)

windowSize :: (Int, Int)
windowSize = (300, 600)

display' :: Display
display' = InWindow "tetris-hs" windowSize (200,200)

main :: IO ()
main = play display' background 60 initialGame gameAsPicture transformGame tick 

-- data Move = MoveLeft | MoveRight | MoveDown | MoveUp | Rotate

-- data Tile = Tile { color :: Color, pos :: (Int, Int) } deriving Eq
-- data Game = Game { tiles :: [Tile], fallT :: Float, tetrominoe :: Tetrominoe }

-- -- data Elo = Elo { tile :: (Int, Int) -> Tile, shape' :: [Int] }
-- -- aa = Elo {
-- --   tile = Tile green,
-- --   shape' = [1]
-- -- }
-- data Tetrominoe = Tetrominoe {
--   color' :: Color,
--   pos'   :: (Int, Int),
--   shape  :: [[Int]]
-- }

-- -- map move to new position
-- movePos :: Move -> (Int, Int) -> (Int, Int)
-- movePos MoveLeft  (x, y) = (x, y-1)
-- movePos MoveRight (x, y) = (x, y+1)
-- movePos MoveDown  (x, y) = (x+1, y)
-- movePos MoveUp    (x, y) = (x-1, y)
-- movePos _         (x, y) = (x, y)

-- -- change tetrominoe's position 
-- moveTetrominoe :: Move -> Tetrominoe -> Tetrominoe
-- moveTetrominoe Rotate t@(Tetrominoe _ _ s) = t { shape = map reverse $ transpose s }
-- moveTetrominoe md     t@(Tetrominoe _ p _) = t { pos'  = movePos md p }

-- -- check if two given list have any common elements
-- haveCommonElements :: Eq a => [a] -> [a] -> Bool
-- haveCommonElements xs ys = not $ null ([ x | x <- xs , y <- ys, x == y])

-- -- check if tetrominoe is inside the bounds
-- overflows :: Move -> Tetrominoe -> Bool
-- overflows MoveLeft  = (>) 0  . minimum . map (snd . pos) . tetrominoeTiles
-- overflows MoveRight = (<) 9  . maximum . map (snd . pos) . tetrominoeTiles
-- overflows MoveDown  = (<) 19 . maximum . map (fst . pos) . tetrominoeTiles
-- overflows _         = const False

-- -- move tetrominoe in opposite direction if he moved out of bounds
-- wallKick :: Tetrominoe -> Tetrominoe
-- wallKick t
--   | overflows MoveLeft  t = wallKick $ moveTetrominoe MoveRight t
--   | overflows MoveRight t = wallKick $ moveTetrominoe MoveLeft  t
--   | overflows MoveDown  t = wallKick $ moveTetrominoe MoveUp    t
--   | otherwise = t

-- -- check if tetrominoe can move in given direction
-- canMove :: Move -> Game -> Bool
-- canMove md g@(Game _ _ t) = not collides && not (overflows md t)
--   where 
--     collides = haveCommonElements (tetrominoeTiles $ moveTetrominoe md t) (tiles g)

-- -- handle tetrominoe move
-- handleMove :: Move -> Game -> Game
-- handleMove md g@(Game _ _ t)
--   | canMove md g = g { tetrominoe = moveTetrominoe md t }
--   | otherwise    = g

-- -- get tiles representing tetrominoe on the screen
-- tetrominoeTiles :: Tetrominoe -> [Tile]
-- tetrominoeTiles (Tetrominoe c (x,y) s) = toTile . filter ((==1) . snd) . zip indexes $ concat s
--   where
--     toTile  = map ((\(a,b) -> Tile { pos = (x+a, y+b), color = c }) . fst)
--     indexes = concat [[(b,a) | a <- [0..2]] | b <- [0..2]]

-- -- clear rows after tetrominoe have been placed
-- clearRows :: Game -> Game
-- clearRows g = g { tiles = concat notFull }
--   where
--     rows    = groupBy ((==) `on` fst . pos) . sortBy (compare `on` fst . pos) $ tiles g
--     notFull = filter ((<10)  . length) rows 
--     fullXs  = map (fst . pos . head) $ filter ((==10) . length) rows
--     newRows = concat notFull

-- -- move tetrominoe down each game tick
-- fall :: Game -> Game
-- fall g@(Game s _ t)
--   | canMove MoveDown g = g { tetrominoe = moveTetrominoe MoveDown t }
--   | otherwise = clearRows $ g { tetrominoe = l, tiles = s ++ tetrominoeTiles t }

