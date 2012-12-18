module QueensProblem (
    arrangeQueens
  , printTable
  ) where

import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import ListAux

--
-- N Queens Puzzle
--
-- Example: mapM_ printTable $ arrangeQueens 8
--

printTable :: [(Int, Int)] -> IO ()
printTable table = do
  let size = length table
  let positions = map fst $ sortBy (compare `on` snd) table
  forM_ positions (\pos -> putStrLn $ intersperse ' ' $ replicate pos '.' ++ "Q" ++ replicate (size - pos - 1) '.')
  putStrLn ""
                     
arrangeQueens n =
  let attacks = attacksMap n
      combinations = map (zip [0..]) 
                   $ filter unique -- filter out combinations containing any duplications
                   $ sequence $ replicate n [0..n-1]
  in filter (secure attacks) combinations
     
secure attacksMap combination = 
  secure' attacksMap combination combination
  where secure' _ _ [] = True
        secure' map combination (coord:coords) =
          if any (`elem` attacks) to then False
          else secure' map combination coords
          where attacks = fromJust $ lookup coord map
                to = delete coord combination

attacksMap :: Int -> [((Int, Int), [(Int, Int)])]
attacksMap n = [((x, y), attacksFrom n (x, y)) | x <- [0..n-1], y <- [0..n-1]]
    
attacksFrom :: Int -> (Int, Int) -> [(Int, Int)]
attacksFrom n (x, y) = 
    [(x, y + dy) | dy <- [-n+1 .. n-1]
                 , y + dy /= x
                 , y + dy >= 0
                 , y + dy < n]
 ++ [(x + dx, y) | dx <- [-n+1 .. n-1]
                 , x + dx /= y
                 , x + dx >= 0
                 , x + dx < n]
 ++ [(x + dx, y + dy) | dx <- [-n+1 .. n-1], dy <- [-n+1 .. n-1]
                      , abs dx == abs dy
                      , x + dx /= x
                      , x + dx >= 0
                      , x + dx < n
                      , y + dy /= y
                      , y + dy >= 0
                      , y + dy < n]
                                
