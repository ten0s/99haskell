---
--- http://www.haskell.org/haskellwiki/99_questions/46_to_50
--- Logic and Codes 
--- 

import Questions3
import BinaryTree
import Data.List (sort, sortBy, group, insertBy)
import Data.Maybe (fromMaybe)

-- Problem 46
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) 
-- which succeed or fail according to the result of their respective operations; e.g. and(A,B) will succeed, 
-- if and only if both A and B succeed.
-- A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).

and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' a b = not $ and' a b

nor' :: Bool -> Bool -> Bool
nor' a b = not $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True  False = True
xor' False True  = True
xor' _      _    = False

-- Логическая импликация - бинарная операция над двумя высказываниями, результат которой принимает значение
-- Ложь в случае истинности высказывания А и ложности высказывания Б и значение Истина во всех остальных случаях.
impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _    _     = True

-- Логическая эквивалентность - бинарная операция над двумя высказываниями, которая принимает значение
-- Истина в случае истиности или ложности обоих высказываний одновременно и значение Ложь во всех остальных случаях.
equ' :: Bool -> Bool -> Bool
equ' True  True  = True
equ' False False = True
equ' _     _     = False

-- Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
-- table (\a b -> (and' a (or' a b))) == 
-- True True True
-- True False True
-- False True False
-- False False False
--

showArgs :: (Show a) => [a] -> String
showArgs [] = ""
showArgs [x] = show x
showArgs (x:xs) = show x ++ " " ++ showArgs xs

print2Ln :: (Bool -> Bool -> Bool) -> [Bool] -> IO ()
print2Ln f xs@(x:y:_) = do
    putStr $ showArgs xs ++ show (f x y)
    putStrLn ""

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    print2Ln f [True,  True]
    print2Ln f [True,  False]
    print2Ln f [False, True]
    print2Ln f [False, False]

-- Problem 47
-- (*) Truth tables for logical expressions (2).
-- Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical
-- expression in the more natural way, as in the example: A and (A or not B). Define operator precedence as usual;
-- i.e. as in Java.
-- table2 (\a b -> a `and'` (a `or'` not b)) ==
-- True True True
-- True False True
-- False True False
-- False False False
-- 
infixr 3 `and'`
infixr 2 `or'`
infixr 3 `nand'`
infixr 2 `nor'`
infixr 2 `xor'`
infixr 2 `impl'`
infixr 2 `equ'`

-- Problem 48
-- (**) Truth tables for logical expressions (3).
-- Generalize problem P47 in such a way that the logical expression may contain any number of logical variables. 
-- Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains
-- the logical variables enumerated in List.
-- tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- True True True True
-- True True False True
-- True False True True
-- True False False True
-- False True True True
-- False True False True
-- False False True True
-- False False False True
--

truthTable :: Int -> [[Bool]]
truthTable n = cartesianProduct $ replicate n [True, False]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = let table = truthTable n
                 results = map (\row -> row ++ [f row]) table
             in mapM_ (putStrLn . showArgs) results
    
-- Problem 49
-- (**) Gray codes.
-- An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
-- n = 1: C(1) = ['0','1'].
-- n = 2: C(2) = ['00','01','11','10'].
-- n = 3: C(3) = ['000','001','011','010','110','111','101','100'].
-- Find out the construction rules and write a predicate with the following specification:
-- gray(N,C) :- C is the N-bit Gray code
----
---- The reflected binary code, also known as Gray code after Frank Gray, is a binary numeral system where two
---- successive values differ in only one bit. The reflected binary code was originally designed to prevent 
---- spurious output from electromechanical switches. Today, Gray codes are widely used to facilitate error
---- correction in digital communications such as digital terrestrial television and some cable TV systems.
---- Related to:
---- Tower Of Hanoi http://mathworld.wolfram.com/TowerofHanoi.html
---- Baguenaudier http://mathworld.wolfram.com/Baguenaudier.html
----
-- Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be
-- used repeatedly?
-- gray 3 == ["000","001","011","010","110","111","101","100"]
-- 

-- 0 0 0 0
-- 0 0 0 1
-- 0 0 1 1
-- 0 0 1 0
-- 
-- 0 1 1 0
-- 0 1 1 1
-- 0 1 0 1
-- 0 1 0 0 
-- 
-- 1 1 0 0
-- 1 1 0 1
-- 1 1 1 1
-- 1 1 1 0
-- 
-- 1 0 1 0
-- 1 0 1 1
-- 1 0 0 1
-- 1 0 0 0
--    
-- 1 0 0 0
-- 1 0 0 1
-- 1 0 1 1
-- 1 0 1 0
-- . . . .
--
-- codes 0 = [                                          ]
-- codes 1 = [                                 gsn (2^0)]
-- codes 2 = [                      gsn (2^1), gsn (2^0)]
-- codes 3 = [           gsn (2^2), gsn (2^1), gsn (2^0)]
-- codes 4 = [gsn (2^3), gsn (2^2), gsn (2^1), gsn (2^0)]
-- 
codes :: Int -> [String]
codes 0 = []
codes n = gsn (2^(n-1)) : codes (n-1)
    where gsn n = concat $ map (replicate n) $ cycle "0110"

gray :: Int -> [String]
gray 0 = []
gray n = map (\x -> map (!!x) (codes n)) [0..2^n-1]

-- Problem 50
-- (***) Huffman codes.
-- We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
-- Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to construct
-- a list hc(S,C) terms, where C is the Huffman code word for the symbol S. In our example, the result could be
-- Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. 
-- The task shall be performed by the predicate huffman/2 defined as follows:
-- % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
-- huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] == 
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
--

cmpTuplesByFreq :: (Char, Int) -> (Char, Int) -> Ordering
cmpTuplesByFreq (_,lf) (_,rf) = lf `compare` rf

cmpNodesByFreq :: Tree (Char, Int) -> Tree (Char, Int) -> Ordering
cmpNodesByFreq l r = let (Branch (_,lf) _ _) = l
                         (Branch (_,rf) _ _) = r
                     in lf `compare` rf
                  
{-
1. Create a leaf node for each symbol and add it to the priority queue.
2. While there is more than one node in the queue:
    - Remove the two nodes of highest priority (lowest probability) from the queue
    - Create a new internal node with these two nodes as children and with probability equal to the sum
      of the two nodes' probabilities.
    - Add the new node to the queue.
3. The remaining node is the root node and the tree is complete.

[('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]

(1) 5:f 9:e 12:c 13:b 16:d 45:a

  14:*
  /  \
5:f  9:e

(2) 12:c 13:b 14:* 16:d 45:a

   25:*
   /  \
12:c 13:b
       
(3) 14:* 16:d 25:* 45:a

       30:*
     __/  \__
    /        \
  14:*      16:d
  /  \
5:f  9:e

(4) 25:* 30:* 45:a

             55:*
        _____/  \______ 
       /               \
     25:*             30:*
     /  \           __/  \__
  12:c 13:b        /        \
                14:*        16:d
                /  \
              5:f  9:e

(5) 45:a 55:*

       100:*
    ___/  \___
   /          \ 
 45:a         55:*
        _____/  \______ 
       /               \
     25:*             30:*
     /  \           __/  \__
  12:c 13:b        /        \
                14:*        16:d
                /  \
              5:f  9:e
            
(6) 100:*

Branch ('*',100)
                 (Branch ('a',45) Empty Empty)  
                 (Branch ('*',55) 
                                  (Branch ('*',25) 
                                                  (Branch ('c',12) Empty Empty) 
                                                  (Branch ('b',13) Empty Empty))
                                  (Branch ('*',30) 
                                                  (Branch ('*',14) 
                                                                   (Branch ('f',5) Empty Empty) 
                                                                   (Branch ('e',9) Empty Empty)) 
                                                  (Branch ('d',16) Empty Empty)))

a:0
b:101
c:100
d:111
e:1101
f:1100
-}

buildTree :: [Tree (Char, Int)] -> Tree (Char, Int)
buildTree [] = error "Invalid input. Priority queue is empty."
buildTree [r] = r
buildTree (f:s:queue) = let (Branch (_,ff) _ _) = f
                            (Branch (_,sf) _ _) = s
                            newNode = Branch ('*',ff+sf) f s
                            newQueue = insertBy cmpNodesByFreq newNode queue
                        in buildTree newQueue
                        
searchTree :: Tree (Char, Int) -> Char -> String -> Maybe String
searchTree Empty _ acc = Nothing
searchTree (Branch (vc,_) l r) c acc = 
    if vc == c then Just acc
    else case searchTree l c (acc ++ "0") of
            Nothing    -> searchTree r c (acc ++ "1")
            (Just res) -> Just res

huffman :: [(Char, Int)] -> [(Char, String)]
huffman charFreq =
    let -- sort the list by frequencies.
        sortedCharFreq = sortBy cmpTuplesByFreq charFreq
        -- create a leaf node for each symbol and add it to the priority queue.
        priorityQueue = map leaf sortedCharFreq
        -- build huffman binary tree.
        huffmanTree = buildTree priorityQueue
        -- encode source list.
        charCode = map (\(c,_) -> (c, fromMaybe "" (searchTree huffmanTree c ""))) charFreq
    in charCode          
