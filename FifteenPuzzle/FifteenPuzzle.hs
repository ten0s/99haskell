module FifteenPuzzle where

import Data.List
import Data.Maybe
import Data.Tuple (swap)
import Data.Function (on)
import Control.Monad.Error
import Debug.Trace

import ListAux
import TimeMeasure

type Tile      = Int
type Pos       = Int
type Side      = Int
type Field     = [Tile]
type ZeroPos   = Pos
type Path      = [ZeroPos]
type Heuristic = Int

data Board = Board {
    field :: Field 
  , path :: Path
}

instance Show Board where
  show (Board field path) = "Board " ++ show field ++ " " ++ show path
                      
instance Eq Board where
  (Board fl _) == (Board fr _) = fl == fr
  
data BoardError = InvalidBoardSide
                | InvalidFieldLength
                | MissingEmptyTile
                | BoardNotSolvable Board  
                | NoSolutionFound Board Board
                | OtherError String  
                  deriving (Show, Eq)
                           
instance Error BoardError where
  strMsg = OtherError

--
-- 4x4
--
start4 = buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,15]   -- [15] 0.0 sec
start4' = buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,0,14,15]  -- [14,15] 0.0 sec
start4'' = buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,0,13,14,15] -- [13,14,15] 0.0 sec
start4''' = buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,15,12,14,13,0] -- [14,13,12,8,9,10,14,13,9,10,14,15,11,10,9,13,14,10,9,8,12,13,14,15] 5.078 sec
start4'''' = buildBoard 4 [1,2,3,4,5,6,7,8,0,14,13,12,15,9,10,11] -- [9,10,14,13,12,8,9,10,14,15,11,7,6,5,9,10,14,13,12,8,9,5,6,7,11,15] 23.578 sec

start4''''' = buildBoard 4 [2,1,4,3,6,7,8,0,5,14,13,12,15,9,10,11] -- [9,10,14,13,12,8,9,10,14,15,11,7,6,5,9,10,14,13,12,8,9,5,6,7,11,15] 460 sec

test4 = buildBoard 4 [4,2,1,14,13,11,3,6,9,0,10,5,8,7,15,12] -- more that an hour
--worst4 = buildBoard 4 [15,14,0,4,11,1,6,13,7,5,8,9,3,2,10,12] -- for different goal configuration

goal4 = buildBoard 4 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0]

boardSide = 4 -- вернуть все назад 463 против 442 -- не имеет смысла :(
-- введение лишнего параметра глубины против постоянного вызова length не дает НИКАКОГО преимущества

buildBoard :: Side -> Field -> Either BoardError Board
buildBoard side field
  | side < 2 = throwError InvalidBoardSide
  | length field /= side * side = throwError InvalidFieldLength
  | otherwise = case elemIndex 0 field of
                  Nothing -> throwError MissingEmptyTile
                  Just zero -> return (Board field [zero])

--
-- Determining solvability (http://www.cs.bham.ac.uk/~mdr/teaching/modules04/java2/TilesSolvability.html)
---- If the grid width is odd, then the number of inversions in a solvable situation is even.
---- If the grid width is even, and the blank is on an even row counting from the bottom 
----   (second-last, fourth-last etc), then the number of inversions in a solvable situation is odd.
---- If the grid width is even, and the blank is on an odd row counting from the bottom 
----   (last, third-last, fifth-last etc) then the number of inversions in a solvable situation is even.
--
isBoardSolvable :: Board -> Bool
isBoardSolvable (Board _ []) = error "isBoardSolvable: empty path!"
isBoardSolvable (Board f (z:_)) = 
  if odd boardSide then even n else even (n + k)
  where n = countInversions (filter (/=0) f)
        k = z `div` boardSide + 1
        
-- fix doSearch 3 goal4 start4
doSearch goal start = do
  g <- goal
  s <- start
  if isBoardSolvable g
  then if isBoardSolvable s
       then search s g
       else throwError (BoardNotSolvable s)
  else throwError (BoardNotSolvable g)
        
search :: Board -> Board -> Either BoardError [ZeroPos]
search start goal = let (result, _) = search' start (boardHeuristic start goal) goal
                    in case result of
                         Nothing    -> throwError (NoSolutionFound start goal)
                         Just board -> return (tail (reverse (path board)))
    
search' :: Board -> Heuristic -> Board -> (Maybe Board, Heuristic)
search' start low goal =
  let high = maxBound
      (result, high') = searchIDAStar start low high goal
  in case result of
       Nothing -> search' start high' goal
       Just _ -> (result, high')

searchIDAStar :: Board -> Heuristic -> Heuristic -> Board -> (Maybe Board, Heuristic)
searchIDAStar board low high goal
  | board == goal = (Just board, high)
  | otherwise = let descendants = permutateBoard board
                in processDescendants descendants low high goal

processDescendants :: [Board] -> Heuristic -> Heuristic -> Board -> (Maybe Board, Heuristic)
processDescendants [] low high _ = (Nothing, high)
processDescendants (b@(Board f p):bs) low high goal = 
  if g' > low
  then if g' < high
       then processDescendants bs low g' goal
       else processDescendants bs low high goal
  else let (result, high') = searchIDAStar b low high goal
       in case result of    
            Nothing -> processDescendants bs low high' goal
            Just _ -> (result, high')
  where g = length p
        h = boardHeuristic b goal
        g' = g + h

countInversions :: Field -> Int
countInversions [] = 0                       
countInversions (x:xs) = (length $ filter (<x) xs) + countInversions xs
        
slideZero :: Board -> ZeroPos -> Board
slideZero (Board _ []) _ = error "slideZero: empty path!"
slideZero (Board field path@(zero:_)) newZero = Board field'' path'
  where tile = field !! newZero
        field' = replaceAt tile field zero
        field'' = replaceAt 0 field' newZero
        path' = newZero : path

permutateBoard :: Board -> [Board]
permutateBoard (Board _ []) = error "permutateBoard: empty path!"
permutateBoard board@(Board _ (zero:exclude:_)) = (map (slideZero board) zeros)
  where zeros = [zero' |
                   let (zx, zy) = index2coord zero,
                   (x, y) <- [(zx-1, zy), (zx, zy-1), (zx+1, zy), (zx, zy+1)],
                   x >= 0 && x < boardSide, y >= 0 && y < boardSide,
                   let zero' = coord2index (x, y),
                   zero' /= exclude]
permutateBoard board@(Board _ (zero:_)) = (map (slideZero board) zeros)
  where zeros = [zero' | 
                   let (zx, zy) = index2coord zero,
                   (x, y) <- [(zx-1, zy), (zx, zy-1), (zx+1, zy), (zx, zy+1)],
                   x >= 0 && x < boardSide, y >= 0 && y < boardSide,
                   let zero' = coord2index (x, y)]
        
index2coord :: Pos -> (Pos, Pos)
index2coord index = swap (index `divMod` boardSide)

coord2index :: (Pos, Pos) -> Pos
coord2index (x, y) = y * boardSide + x

boardHeuristic :: Board -> Board -> Heuristic
boardHeuristic = manhattanDistance

manhattanDistance :: Board -> Board -> Heuristic
manhattanDistance board goal = boardsDistance board goal

boardsDistance :: Board -> Board -> Heuristic
boardsDistance (Board lf _) (Board rf _) = sum $ zipWith distance lf rf
  where distance l r | r == 0 = 0 -- assummed to be the goal
                     | l == r = 0
                     | otherwise = let (lx, ly) = index2coord (fromJust (elemIndex l lf))
                                       (rx, ry) = index2coord (fromJust (elemIndex l rf))
                                   in abs (lx - rx) + abs (ly - ry)
                               
