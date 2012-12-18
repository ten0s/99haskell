----
---- http://www.haskell.org/haskellwiki/99_questions/61_to_69
---- Binary trees, continued 
---- 

{-# LANGUAGE CPP #-} -- to use __LINE__

module Questions7 where

import Questions6
import Debug
import Debug.Trace
import Label
import BinaryTree

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

-- Problem 61
-- Count the leaves of a binary tree
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
-- countLeaves tree4 == 2
-- 
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- Problem 61A
-- Collect the leaves of a binary tree in a list
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect them in a list.
-- leaves tree4 == [4,2]
-- 
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r) = leaves l ++ leaves r

-- Problem 62
-- Collect the internal nodes of a binary tree in a list
-- An internal node of a binary tree has either one or two non-empty successors. 
-- Write a predicate internals/2 to collect them in a list.
-- internals tree4 == [1,2]
-- 
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v l r) = [v] ++ internals l ++ internals r

-- Problem 62B
-- Collect the nodes at a given level in a list
-- A node of a binary tree is at level N if the path from the root to the node has length N-1.
-- The root node is at level 1. Write a predicate atlevel/3 to collect all nodes at a given level in a list.
-- atLevel tree4 2 == [2,2]
-- 
atLevel :: Tree a -> Int -> [a]
atLevel tree level = atLevel' tree 1 []
    where 
        atLevel' Empty _ acc = acc
        atLevel' (Branch v l r) curLevel acc
            | curLevel == level = v : acc
            | curLevel < level  = atLevel' r (curLevel+1) (atLevel' l (curLevel+1) acc)

-- Problem 63
-- Construct a complete binary tree
-- A complete binary tree with height H is defined as follows:
-- The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
-- In level H, which may contain less than the maximum possible number of nodes, all the nodes are
-- "left-adjusted". This means that in a level order tree traversal all internal nodes come first, 
-- the leaves come second, and empty successors (the nil's which are not really nodes!) come last. 
-- Particularly, complete binary trees are used as data structures (or addressing schemes) for heaps.
-- We can assign an address number to each node in a complete binary tree by enumerating the nodes in level-order,
-- starting at the root with number 1. For every node X with address A the following property holds:
-- The address of X's left and right successors are 2*A and 2*A+1, respectively, if they exist. This fact can be
-- used to elegantly construct a complete binary tree structure.
-- Write a predicate complete_binary_tree/2.
-- completeBinaryTree 4 == 
-- Branch 'x' (Branch 'x' (Branch 'x' Empty Empty) Empty) (Branch 'x' Empty Empty)
-- isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty) == True
-- 

isCompleteBinaryTree :: (Eq a) => Tree a -> Bool
isCompleteBinaryTree tree = let height = treeHeight tree
                                count = branchCount tree
                            in maxBranches height == count

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = buildCBT n (\_ -> 'x')
  
-- Problem 64
-- Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for drawing the tree,
-- a layout algorithm is required to determine the position of each node in a rectangular grid. 
-- Several layout methods are conceivable, one of them is shown in the illustration below:
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p64.gif
-- In this layout strategy, the position of a node v is obtained by the following two rules:
--  x(v) is equal to the position of the node v in the inorder sequence
--  y(v) is equal to the depth of the node v in the tree 
-- Write a function to annotate each node of the tree with a position, where (1,1) in the top left corner or 
-- the rectangle bounding the drawn tree.
-- Here is the example tree from the above illustration:
{-
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )
-}
-- layout tree64 == Branch ('n',(8,1)) (Branch ('k',(6,2)) (Branch ('c',(2,3)) ...
-- 

-- Problem 65
-- An alternative layout method is depicted in the illustration below:
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p65.gif
-- Find out the rules and write the corresponding function. Hint: On a given level, the horizontal distance
-- between neighboring nodes is constant.
-- Use the same conventions as in problem P64 and test your function in an appropriate way.
-- Here is the example tree from the above illustration:
{-
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
-}
-- layout tree65 == Branch ('n',(15,1)) (Branch ('k',(7,2)) (Branch ('c',(3,3)) ...
-- 

-- Problem 66
-- Yet another layout strategy is shown in the illustration below:
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p66.gif
-- The method yields a very compact layout while maintaining a certain symmetry in every node. 
-- Find out the rules and write the corresponding Prolog predicate. Hint: Consider the horizontal distance 
-- between a node and its successor nodes. How tight can you pack together two subtrees to construct the
-- combined binary tree?
-- Use the same conventions as in problem P64 and P65 and test your predicate in an appropriate way. 
-- Note: This is a difficult problem. Don't give up too early!
-- Which layout do you like most?
-- layout tree65 == Branch ('n',(5,1)) (Branch ('k',(3,2)) (Branch ('c',(2,3)) ...
-- 

-- Problem 67A
-- A string representation of binary trees
-- Somebody represents binary trees as strings of the following type:
-- a(b(d,e),c(,f(g,))) 
-- https://sites.google.com/site/prologsite/prolog-problems/4/p67.gif
-- a) Write a Prolog predicate which generates this string representation, 
-- if the tree is given as usual (as nil or t(X,L,R) term). 
-- b) Then write a predicate which does this inverse; i.e. given the string representation, 
-- construct the tree in the usual form. 
-- c) Finally, combine the two predicates in a single predicate tree_string/2 which can be used in both directions.
-- return (stringToTree "x(y,a(,b))") >>= print
-- == Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))
--
-- let t = cbtFromList ['a'..'z'] in return (stringToTree (treeToString t)) >>= print . (== t) 
-- == True
-- For simplicity, suppose the information in the nodes is a single letter and there are no spaces in the string. 
-- But it would be great to implement something like:
-- parseTree :: (Read a) => String -> (Maybe (Tree a), String)

treeToString :: (Label a, Show a) => Tree a -> String
treeToString Empty = ""
treeToString (Branch v Empty Empty) = label v
treeToString (Branch v l r) = label v ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: String -> Tree Char
stringToTree xs = t
    where (t, ys) = parseTree xs

parseTree :: String -> (Tree Char, String)
parseTree ""         = (Empty, "")
parseTree (v:[])     = (Branch v Empty Empty, "")
parseTree (v:'(':xs) = let ((l, r), ys) = parseChildren xs
                       in (Branch v l r, ys)
parseTree all@(_)    = trace ("\n" ++ __FILE__ ++ ":" ++ show __LINE__ ++ " Unparsed: " ++ all) undefined

parseChildren :: String -> ((Tree Char, Tree Char), String)
parseChildren xs = let (l, ys) = parseLeftChild xs
                       (r, zs) = parseRightChild ys
                   in ((l, r), zs)
                       
parseLeftChild :: String -> (Tree Char, String)
parseLeftChild (',':xs)   = (Empty, xs)
parseLeftChild (v:',':xs) = (Branch v Empty Empty, xs)
parseLeftChild (v:'(':xs) = let ((l, r), ',':ys) = parseChildren xs
                            in (Branch v l r, ys)
parseLeftChild all@(_)    = trace ("\n" ++ __FILE__ ++ ":" ++ show __LINE__ ++ " Unparsed: " ++ all) undefined
                          
parseRightChild :: String -> (Tree Char, String)
parseRightChild (')':xs)   = (Empty, xs)
parseRightChild (v:')':xs) = (Branch v Empty Empty, xs)
parseRightChild (v:'(':xs) = let ((l, r), ')':ys) = parseChildren xs
                             in (Branch v l r, ys)
parseRightChild all@(_)    = trace ("\n" ++ __FILE__ ++ ":" ++ show __LINE__ ++ " Unparsed: " ++ all) undefined

--
-- Test function
--
testParser :: String -> Bool
testParser string = let tree = stringToTree string 
                    in treeToString tree == string
              
--
-- Evaluate `c' in GHCi to test
--              
c = and $ map testParser ["", "1", "1(2,)", "1(2,3)", "1(,3)", "1(2(4,5),3)", "1(2(4,5),3(6,7))"]

cbtFromList :: [a] -> Tree a
cbtFromList = foldl (\tree x -> insertLeafToCBT (leaf x)  tree) Empty

-- Problem 68
-- Preorder and inorder sequences of binary trees. We consider binary trees with nodes that are identified
-- by single lower-case letters, as in the example of problem P67.
-- a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence of a given
-- binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the preorder sequence of the
-- example in problem P67.
-- b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder sequence,
-- construct a corresponding tree? If not, make the necessary arrangements.
-- c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are given,
-- then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.
--
-- The following procedure demonstrates on how to rebuild tree from given inorder and preorder traversals 
-- of a binary tree:
--  - Preorder traversal visits Node, left subtree, right subtree recursively
--  - Inorder traversal visits left subtree, node, right subtree recursively
--  - Since we know that the first node in Preorder is its root, we can easily locate the root node in the
--    inorder traversal and hence we can obtain left subtree and right subtree from the inorder traversal
--    recursively
--
{- 
let { Just t = stringToTree "a(b(d,e),c(,f(g,)))"; po = treeToPreorder t; io = treeToInorder t } in preInTree po io >>= print
Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
-}
--

treeToPreorder :: Tree a -> [a]
treeToPreorder Empty          = []
treeToPreorder (Branch v l r) = v : treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree a -> [a]
treeToInorder Empty          = []
treeToInorder (Branch v l r) = treeToInorder l ++ [v] ++ treeToInorder r

preInTree :: (Eq a) => [a] -> [a] -> Tree a
preInTree [] _   = Empty
preInTree po io = let (rootPO:tailPO)     = po
                      (leftIO, _:rightIO) = span (/= rootPO) io
                      (leftPO, rightPO)   = span (`elem` leftIO) tailPO
                  in Branch rootPO (preInTree leftPO leftIO) (preInTree rightPO rightIO)
          
c' = let t = stringToTree "a(b(d,e),c(,f(g,)))"
         po = treeToPreorder t
         io = treeToInorder t
         t' = preInTree po io
     in t == t'

-- Problem 69
-- Dotstring representation of binary trees.
-- We consider again binary trees with nodes that are identified by single lower-case letters,
-- as in the example of problem P67. Such a tree can be represented by the preorder sequence of its nodes in
-- which dots (.) are inserted where an empty subtree (nil) is encountered during the tree traversal.
-- For example, the tree shown in problem P67 is represented as 'abd..e..c.fg...'. First, try to establish
-- a syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which does the conversion in
--  both directions. Use difference lists.
-- fst (ds2tree example) 
-- == Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))
--
-- tree2ds (Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty))
-- == "xy..z0..."
--

ds2tree :: String -> (Tree Char, String)
ds2tree "" = (Empty, "")
ds2tree ('.':xs) = (Empty, xs)
ds2tree (x:xs) = let ((l, r), ys) = children xs
                 in (Branch x l r, ys)
    where children cs = let (l, ds) = ds2tree cs
                            (r, es) = ds2tree ds
                        in ((l, r), es)


tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch v l r) = [v] ++ tree2ds l ++ tree2ds r

c'' = let input = "xy..z0..."
          tree = fst $ ds2tree input
          test = tree2ds tree
      in input == test
