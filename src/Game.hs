{-# LANGUAGE InstanceSigs #-}
module Game where

import Graphics.Gloss (Color, green)
import Data.Maybe (catMaybes)
import Data.Matrix (mapPos, fromLists, toList, fromList, transpose, toLists)

data Block = Block { color :: Color, pos :: (Int, Int) } 

instance Eq Block where 
  (==) :: Block -> Block -> Bool
  (==) (Block _ p1) (Block _ p2) = p1 == p2

data Game = Game  { fallTime :: Float,  blocks :: [Block], tetrominoe :: [[Maybe Block]] }
data Move = MoveLeft | MoveRight | MoveDown

move :: Move -> (Int, Int) -> (Int, Int)
move MoveLeft  (x, y) = (x, y-1)
move MoveRight (x, y) = (x, y+1)
move MoveDown  (x, y) = (x+1, y)

moveBlock :: Move -> Maybe Block -> Maybe Block
moveBlock md (Just (Block c p)) = Just $ Block c (move md p)
moveBlock _ Nothing = Nothing

moveTetrominoe :: Move -> [[Maybe Block]] -> [[Maybe Block]]
moveTetrominoe md = map (map (moveBlock md)) 

-- rotateTetrominoe :: [[Maybe Block]] -> [[Maybe Block]]
-- rotateTetrominoe t = fromList t

handleMove :: Move -> Game -> Game
handleMove md g@(Game _ b t)
  | transformConditions = g { tetrominoe = tt }
  | otherwise = g
  where
    tt = moveTetrominoe md t
    transformConditions = tt `isInside` (20,10) && tt `notCollidesWith` b

isInside :: [[Maybe Block]] -> (Int, Int) -> Bool
isInside t (r, c) = all (posInside . pos) . catMaybes $ concat t
  where
    posInside (x, y) = x `elem` [0..r-1] && y `elem` [0..c-1]

notCollidesWith :: [[Maybe Block]] -> [Block] -> Bool
notCollidesWith t b = not . any (`elem` b) . catMaybes $ concat t

tick :: Float -> Game -> Game
tick et g@(Game ft _ _)
  | 1 <= ft   = (handleMove MoveDown g) { fallTime = 0 }
  | otherwise = g { fallTime = ft + (et * 8)}

initialGame :: Game
initialGame = Game {
  fallTime   = 0,
  blocks     = [],
  tetrominoe = l
}

l :: [[Maybe Block]]
l = makeTetrominoe green lShape

makeTetrominoe :: Color -> [[Int]] -> [[Maybe Block]]
makeTetrominoe c = toLists . mapPos intToBlock . fromLists 
  where 
    intToBlock (x,y) 1 = Just $ Block c (x-1, y+2) 
    intToBlock _ _ = Nothing

lShape :: [[Int]]
lShape = [[0,1,1],
          [0,1,0],
          [0,1,0]]
