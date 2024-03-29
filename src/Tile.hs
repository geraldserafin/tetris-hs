{-# LANGUAGE InstanceSigs #-}
module Tile where

import qualified Graphics.Gloss.Data.Point.Arithmetic as P ((+))
import Graphics.Gloss

data Tile = Tile { color' :: Color, pos :: Point }
data RotationDirection = RotateLeft | RotateRight

instance Eq Tile where
  (==) :: Tile -> Tile -> Bool
  (==) (Tile _ p1) (Tile _ p2) = p1 == p2

moveTile :: Point -> Tile -> Tile
moveTile p1 (Tile c p2) = Tile c (p1 P.+ p2)

rotateTile :: RotationDirection -> Point -> Tile -> Tile
rotateTile RotateRight (cx, cy) (Tile e (x,y)) = Tile e (y-cy+cx , negate (x-cx)+cy)
rotateTile RotateLeft  (cx, cy) (Tile e (x,y)) = Tile e (negate (y-cy)+cx, x-cx+cy)
