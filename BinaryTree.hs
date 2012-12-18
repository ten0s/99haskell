module BinaryTree ( 
    Tree(..)
  , leaf
  , branchCount
  , treeHeight
  , maxBranches
  , minHeight
  , binPath
  , path
  , pathWithLength
  , insertLeafToCBT
  , insertLeafWithPath
  , buildCBT
  ) where

import Debug.Trace
import Data.List (find)
import Data.Maybe (fromMaybe)

data Tree a = Empty 
            | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
              
leaf :: a -> Tree a
leaf v = Branch v Empty Empty

branchCount :: Tree a -> Int
branchCount Empty = 0
branchCount (Branch _ left right) = 1 + branchCount left + branchCount right

treeHeight :: (Eq a) => Tree a -> Int
treeHeight Empty = 0
treeHeight (Branch _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- maximum branches at `h' height. !!! Root branch has height 1 !!!
maxBranches :: (Integral a) => a -> a
maxBranches h = 2 ^ h - 1

-- minimum height containing `n' nodes. !!! Root branch has height 1 !!!
minHeight :: (Integral a) => a -> a
minHeight n = ceiling (logBase 2 ((fromIntegral n) + 1))

data Direction = LeftTurn
               | RightTurn
                 deriving (Eq)
                 
instance Show Direction where
    show LeftTurn  = "/"
    show RightTurn = "\\"
    
generateBinaryPath :: a -> a -> [[[a]]]
generateBinaryPath l r = iterate (\path -> binaryPath l r path) []
    where binaryPath l r []   = [[l],[r]]
          binaryPath l r path = map (\d -> l : d) path 
                             ++ map (\d -> r : d) path

-- [ []
-- , [[/],[\]]
-- , [[/,/],[/,\],[\,/],[\,\]]
-- , [[/,/,/],[/,/,\],[/,\,/],[/,\,\],[\,/,/],[\,/,\],[\,\,/],[\,\,\]]
-- , ...]
binPath :: [[[Direction]]]
binPath = generateBinaryPath LeftTurn RightTurn

-- path (3, 1) == [/,/,\]
path :: (Int, Int) -> [Direction]
path (0, _)           = []
path (section, index) = binPath !! section !! index

-- pathWithLength 3 == [[/,/,/],[/,/,\],[/,\,/],[/,\,\],[\,/,/],[\,/,\],[\,\,/],[\,\,\]]
pathWithLength :: Int -> [[Direction]]
pathWithLength 0   = []
pathWithLength len = fromMaybe [] $ find (\lst -> (length lst > 0) && (length (head lst) == len)) binPath

--
-- Insert the given leaf to the given complete binary tree (CBT) to the most right position in left to right order.
--
insertLeafToCBT :: Tree a -> Tree a -> Tree a
insertLeafToCBT leaf tree = let countAfter = branchCount tree + 1
                                heightAfter = minHeight countAfter
                                section = heightAfter - 1
                                maxLeafAtHeight = 2^section
                                index = countAfter - maxLeafAtHeight
                                treePath = path (section, index)
                            in insertLeafWithPath leaf tree treePath
                       
--
-- Insert the given leaf to the binary tree to a position described by path.
--
insertLeafWithPath :: Tree a -> Tree a -> [Direction] -> Tree a
insertLeafWithPath leaf _              []       = leaf
insertLeafWithPath leaf (Branch v l r) (p:path) = if p == LeftTurn
                                                  then Branch v (insertLeafWithPath leaf l path) r
                                                  else Branch v l (insertLeafWithPath leaf r path)
            
--
-- Build complete binary tree (CBT) consisting of `n' elements,
-- using `f' function to build node's value.
--
buildCBT :: Int -> (Int -> a) -> Tree a
buildCBT n f = foldl (\t l -> insertLeafToCBT l t) Empty [leaf (f x) | x <- [1..n]]