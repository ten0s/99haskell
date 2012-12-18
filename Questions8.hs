---
--- http://www.haskell.org/haskellwiki/99_questions/70B_to_73
--- Multiway Trees 
---

module Questions8 where

import Debug.Trace
import Control.Monad
import Control.Monad.Error -- Either instance of Monad

data Tree a = Node a [Tree a]
              deriving (Eq, Show)
              
tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []] 
tree3 = Node 'a' [Node 'b' [Node 'c' []]] 
tree4 = Node 'b' [Node 'd' [], Node 'e' []] 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                 ]

-- Problem 70B
-- (*) Check whether a given term represents a multiway tree.
-- 
-- No solution is needed:
-- As in problem 54A, all members of this type are multiway trees; there is no use for a predicate to test them.
-- 

-- Problem 70C
-- (*) Count the nodes of a multiway tree.
-- nnodes tree2 == 2
-- 
nnodes :: Tree a -> Int
nnodes (Node _ nodes) = 1 + sum (map nnodes nodes)

-- Problem 70
-- (**) Tree construction from a node string.
-- We suppose that the nodes of a multiway tree contain single characters. In the depth-first order sequence of its
-- nodes, a special character ^ has been inserted whenever, during the tree traversal, the move is a backtrack 
-- to the previous level.
-- By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p70.gif
-- Define the syntax of the string and write a predicate tree(String,Tree) to construct the Tree when
-- the String is given. Make your predicate work in both directions.
-- 
tree :: String -> Either String (Tree Char)
tree "" = Left "Invalid input."
tree cs = liftM head (tree' cs [])

-- 1. tree' "afg^^c^bd^e^^" []
-- 2. tree' "fg^^c^bd^e^^" [Node 'a' []]
-- 3. tree' "g^^c^bd^e^^" [Node 'f', [], Node 'a' []]
-- 4. tree' "^^c^bd^e^^" [Node 'g' [], Node 'f' [], Node 'a' []]
-- 5. tree' "^c^bd^e^^" [Node 'f' [Node 'g' []], Node 'a' []]
-- 6. tree' "c^bd^e^^" [Node 'a' [Node 'f' [Node 'g' []]]]
-- 7. tree' "^bd^e^^" [Node 'c' [], Node 'a' [Node 'f' [Node 'g' []]]]
-- ...
tree' :: String -> [Tree Char] -> Either String ([Tree Char])
tree' [] nodes = Right nodes
tree' ('^':cs) (head:(Node sv snodes):nodes) = tree' cs (Node sv (snodes ++ [head]) : nodes)
tree' (c:cs) nodes = tree' cs ((Node c []) : nodes)

-- Problem 71
-- (*) Determine the internal path length of a tree.
-- We define the internal path length of a multiway tree as the total sum of the path lengths from the root to
-- all nodes of the tree. By this definition, tree5 has an internal path length of 9.
-- ipl tree5 == 9
-- ipl tree4 == 2
-- 
ipl :: Tree a -> Int
ipl t = ipl' 0 t
    where ipl' acc (Node _ nodes) = acc + sum (map (ipl' (acc + 1)) nodes)

-- Problem 72
-- (*) Construct the bottom-up order sequence of the tree nodes.
-- Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the
-- multiway tree Tree.
-- bottom_up tree5 == "gfcdeba"
-- 
bottom_up :: Tree a -> [a]
bottom_up (Node v nodes) = concatMap bottom_up nodes ++ [v]

-- Problem 73
-- (**) Lisp-like tree representation.
-- There is a particular notation for multiway trees in Lisp. Lisp is a prominent functional programming language,
-- which is used primarily for artificial intelligence problems. As such it is one of the main competitors 
-- of Prolog. In Lisp almost everything is a list, just as in Prolog everything is a term.
-- The following pictures show how multiway tree structures are represented in Lisp.
-- https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p73.png
-- Note that in the "lispy" notation a node with successors (children) in the tree is always the first element
-- in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence of atoms
-- and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this sequence of
-- tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the Prolog
-- list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the "lispy token list"
-- LTL if the tree is given as term T in the usual Prolog notation.
--
-- display lisp tree1 == "a"
-- display lisp tree2 == "(a b)"
-- display lisp tree3 == "(a (b c))"
-- display lisp tree4 == "(b d e)"
-- display lisp tree5 == "(a (f g) c (b d e))"
-- 
lisp :: Tree Char -> String
lisp (Node x []) = [x]
lisp (Node x nodes) = "(" ++ [x] ++ " " ++ processNodes nodes ++ ")"
    where processNodes [] = ""
          processNodes [node] = lisp node
          processNodes (node:nodes) = lisp node ++ " " ++ processNodes nodes

-- As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse conversion
-- is also possible.
unlisp :: String -> Either String (Tree Char)
unlisp []  = Left "No input."
unlisp [x] = Right (Node x [])
unlisp xs  = liftM (head . fst) (parse xs [])

parse :: String -> [Tree Char] -> Either String ([Tree Char], String)
parse ('(':xs) acc = case parse xs [] of
                        Left err         -> Left err
                        Right (acc', ys) -> case parse ys acc of
                                               Left err          -> Left err
                                               Right (acc'', zs) -> case acc' of
                                                                       []               -> Right (acc'', zs)
                                                                       (Node x []:rest) -> Right ((Node x rest) : acc'', zs)
                                                                       otherwise        -> Left "Invalid input."

parse (')':xs) acc = Right (acc, xs)

parse (' ':xs) acc = parse xs acc

parse (x:xs) acc = case parse xs acc of
                      Left err         -> Left err
                      Right (acc', ys) -> Right (Node x [] : acc', ys)
                   
parse [] acc = Right (acc, "")

c = let s = "(a (f (g h)) (c x y z) (b d e))"
        (Right t) = unlisp s
        s' = lisp t
    in s == s'
        
