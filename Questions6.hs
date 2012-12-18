----
---- http://www.haskell.org/haskellwiki/99_questions/54A_to_60
---- Binary trees 
----

module Questions6 where

import BinaryTree
import Data.List (nub, sort)

-- Problem 54A
-- (*) Check whether a given term represents a binary tree
--
-- No solution is needed:
-- Haskell's type system ensures that all terms of type Tree a are binary trees: 
-- it is just not possible to construct an invalid tree with this type. Hence, it is redundant to 
-- introduce a predicate to check this property: it would always return True.


-- Problem 55
-- (**) Construct completely balanced binary trees
-- In a completely balanced binary tree, the following property holds for every node: 
-- The number of nodes in its left subtree and the number of nodes in its right subtree are almost equal, 
-- which means their difference is not greater than one.
-- 
-- Write a function cbal-tree to construct completely balanced binary trees for a given number of nodes.
-- The predicate should generate all solutions via backtracking. Put the letter 'x' as information into all
-- nodes of the tree.
-- Example in Haskell, whitespace and "comment diagrams" added for clarity and exposition:
-- cbalTree 4
{-
[
-- permutation 1
--     x
--    / \
--   x   x
--        \
--         x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)),
 
-- permutation 2
--     x
--    / \
--   x   x
--      /
--     x
Branch 'x' (Branch 'x' Empty Empty) 
           (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty),
 
-- permutation 3
--     x
--    / \
--   x   x
--    \
--     x
Branch 'x' (Branch 'x' Empty 
                       (Branch 'x' Empty Empty)) 
           (Branch 'x' Empty Empty),
 
-- permutation 4
--     x
--    / \
--   x   x
--  /
-- x
Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) 
                       Empty) 
           (Branch 'x' Empty Empty)
]
-}
--
cbalTreeFn :: (Eq a) => Int -> (Int -> a) -> [Tree a]
cbalTreeFn 0 f = [Empty]
cbalTreeFn n f = nub $ concat $ map permutate $ cbalTreeFn (n-1) f
    where newLeaf = leaf (f n)
          permutate Empty                  = [newLeaf]
          permutate (Branch v Empty Empty) = [Branch v newLeaf Empty, Branch v Empty newLeaf]
          permutate (Branch v Empty right) = [Branch v newLeaf right]
          permutate (Branch v left Empty)  = [Branch v left newLeaf]
          permutate (Branch v left right)  = map (\t -> Branch v t right) (permutate left) ++ 
                                             map (\t -> Branch v left t) (permutate right)

cbalTree n = cbalTreeFn n (\_ -> 'x') -- cbalTree 3 == [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]
cbalTree' n = cbalTreeFn n id         -- >cbalTree' 3 == [Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty),Branch 1 (Branch 3 Empty Empty) (Branch 2 Empty Empty)]

-- Problem 56
-- (**) Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a vertical line through the root node and
-- then the right subtree is the mirror image of the left subtree. Write a predicate symmetric/1 to check 
-- whether a given binary tree is symmetric. 
-- Hint: Write a predicate mirror/2 first to check whether one tree is the mirror image of another.
-- We are only interested in the structure, not in the contents of the nodes.
-- symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty) == False
-- symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)) == True
--
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror Empty _     = False
mirror _     Empty = False
mirror (Branch _ ll lr) (Branch _ rl rr) = mirror ll rr && mirror lr rl

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ left right) = mirror left right

-- Problem 57
-- (**) Binary search trees (dictionaries)
-- Use the predicate add/3, developed in chapter 4 of the course, to write a predicate to construct a binary search
-- tree from a list of integer numbers.
-- Then use this predicate to test the solution of the problem P56.
-- construct [3, 2, 5, 7, 1] == Branch 3 (Branch 2 (Branch 1 Empty Empty) Empty) (Branch 5 Empty (Branch 7 Empty Empty))
--
-- symmetric . construct $ [5, 3, 18, 1, 4, 12, 21] == True
-- symmetric . construct $ [3, 2, 5, 7, 1] == True
-- 
insertValue :: (Ord a) => a -> Tree a -> Tree a
insertValue value Empty = leaf value
insertValue value (Branch v l r) = if value <= v
                                   then Branch v (insertValue value l) r
                                   else Branch v l (insertValue value r)
                                   
construct :: (Ord a) => [a] -> Tree a
construct xs = construct' xs Empty
    where construct' [] tree = tree
          construct' (y:ys) tree = construct' ys (insertValue y tree)

-- Problem 58
-- (**) Generate-and-test paradigm
-- Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees with
--  a given number of nodes.
-- symCbalTrees 5 == [Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' (Branch 'x' Empty Empty) Empty),Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty (Branch 'x' Empty Empty))]
-- 
symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetric $ cbalTree n

-- Problem 59
-- (**) Construct height-balanced binary trees
-- In a height-balanced binary tree, the following property holds for every node: 
-- The height of its left subtree and the height of its right subtree are almost equal, 
-- which means their difference is not greater than one.
-- 
-- Write a predicate hbal_tree/2 to construct height-balanced binary trees for a given height.
-- The predicate should generate all solutions via backtracking. Put the letter 'x' as information into
-- all nodes of the tree.
-- take 4 $ hbalTree 'x' 3 == !!! ten0s: According to the `Introduction to Algorithms' the tree's root height equals to 0, not 1 !!!
-- [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) Empty),
--  Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)),
--  Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)]
--  
hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x h = [Branch x l r |
        (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
        l <- hbalTree x hl, r <- hbalTree x hr]

-- Problem 60
-- (**) Construct height-balanced binary trees with a given number of nodes
-- Consider a height-balanced binary tree of height H. What is the maximum number of nodes it can contain?
-- Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more difficult.
-- Try to find a recursive statement and turn it into a function minNodes that returns the minimum number of 
-- nodes in a height-balanced binary tree of height H. On the other hand, we might ask: what is the maximum
-- height H a height-balanced binary tree with N nodes can have? Write a function maxHeight that computes this.
-- Now, we can attack the main problem: construct all the height-balanced binary trees with a given number of nodes.
-- Find out how many height-balanced trees exist for N = 15.
-- length $ hbalTreeNodes 'x' 15 == 1553
-- map (hbalTreeNodes 'x') [0..3] ==
-- [[Empty],
--  [Branch 'x' Empty Empty],
--  [Branch 'x' Empty (Branch 'x' Empty Empty),Branch 'x' (Branch 'x' Empty Empty) Empty],
--  [Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)]]
-- 

{- ten0s: Not solved yet! -}

