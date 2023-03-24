module Shuffle where
import Control.Monad.Random (RandomGen, Random (randomR))
import Tetromino (Tetromino, tetrominos)

remove :: Int -> [a] -> [a]
remove _ [] = []
remove 0 (_:xs) = xs
remove n (x:xs) = x : remove (n-1) xs

shuffle :: RandomGen g => [a] -> g -> ([a], g)
shuffle [] = (,) []
shuffle ls = shuffle' ([], ls)
  where
    shuffle' (v, []) g = (v, g) 
    shuffle' (v, resource) g = shuffle' (resource !! index : v, remove index resource) g'
      where 
        (index, g') = randomR (0, length resource - 1) g

infiniteTetrominos :: RandomGen g => g -> [Tetromino]
infiniteTetrominos g = v ++ infiniteTetrominos g' 
  where 
    (v, g') = shuffle tetrominos g