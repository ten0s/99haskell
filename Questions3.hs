---
--- http://www.haskell.org/haskellwiki/99_questions/21_to_28
--- Lists
---

module Questions3 where

import Data.List (splitAt, nub, permutations, sortBy, sort, group, partition, subsequences, intersect, (\\))
import Data.Maybe (fromMaybe)
import System.Random
import Control.Applicative

-- Problem 21
-- Insert an element at a given position into a list.
-- insertAt 'X' "abcd" 2 == "aXbcd"
--
insertAt :: a -> [a] -> Int -> [a]
insertAt elem list index = (\(before, after) -> before ++ (elem : after)) $ splitAt (index - 1) list

-- Problem 22
-- Create a list containing all integers within a given range.
-- range 4 9 == [4,5,6,7,8,9]
--
range :: Int -> Int -> [Int]
range lo hi = [lo..hi]

-- Problem 23
-- Extract a given number of randomly selected elements from a list.
-- Hint: use System.Random
-- rnd_select "abcdefgh" 3 >>= putStrLn == eda
-- 
rnd_select :: [a] -> Int -> IO [a]
rnd_select list count = do
	gen <- newStdGen
	let indices = take count $ (randomRs (0, length list - 1) gen)
	return (map (\i -> list !! i) indices)

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
-- diff_select 6 49 == [23,1,17,33,21,37]
--
diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
	gen <- newStdGen
	return (take n $ nub $ randomRs (1, m) gen)

-- Problem 25
-- Generate a random permutation of the elements of a list.
-- rnd_permu "abcdef" == "badcef"
--
rnd_permu :: [a] -> IO [a]
rnd_permu list = do
	let perms = permutations list
	index <- randomRIO (0, length perms)
	return (perms !! index)
	
-- Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are
-- C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure
-- mathematicians, this result may be great. But we want to really generate all the possibilities 
-- in a list.
-- combinations 3 "abcdef" == ["abc","abd","abe",...]
--
combinations :: (Ord a) => Int -> [a] -> [[a]]
combinations k set = sort $ fst $ partition (\xs -> length xs == k) $ subsequences set

-- Problem 27
-- Group the elements of a set into disjoint subsets.
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? 
--    Write a function that generates all the possibilities and returns them in a list.
-- group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] == 
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...] 
-- (altogether 1260 solutions)
--
{-
input = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
subs = subsequences input
subs1 = [x | x <- subs, length x == 2]
subs2 = [x | x <- subs, length x == 3]
subs3 = [x | x <- subs, length x == 4]
res = [[x,y,z] | x <- subs1, y <- subs2, z <- subs3, x `intersect` y == [], y `intersect` z == [], x `intersect` z == []]
-}

-- see below

-- b) Generalize the above predicate in a way that we can specify a list of group sizes and 
--    the predicate will return a list of groups. Note that we do not want permutations of the group
--    members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). 
--    However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and 
--    ((CARLA DAVID) (ALDO BEAT) ...). You may find more about this combinatorial problem in a good book
--    on discrete mathematics under the term "multinomial coefficients".
-- group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"] == 
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)
--

-- !!! It works! :) But it is not effective by any means :( !!!
myGroup :: (Eq a) => [Int] -> [a] -> [[[a]]]
myGroup lengths set = let subs = subsequences set -- form all subsequences from the given set
                          subsLenGroups = map (\len -> [group | group <- subs, length group == len]) lengths -- group subsequences to groups be lengths
                          cartProd = cartesianProduct subsLenGroups -- make all possible cartesian product from groupped subsequences
                      in filter unique cartProd
                  
unique :: (Eq a) => [[a]] -> Bool
unique xss = let sum = foldl (++) [] xss 
                 uniq = nub sum
              in sum == uniq
              
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (set:sets) = let cp = cartesianProduct sets 
                              in [x : xs | x <- set, xs <- cp]

-- Problem 28
-- Sorting a list of lists according to length of sublists
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the 
--    elements of this list according to their length. E.g. short lists first, longer lists later, or
--    vice versa.
-- lsort ["abc","de","fgh","de","ijkl","mn","o"] == ["o","de","de","mn","abc","fgh","ijkl"]
-- 
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> length xs `compare` length ys) 

-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the 
--    objective is to sort the elements of this list according to their length frequency; 
--    i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
--    others with a more frequent length come later.
-- lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"] == ["ijkl","o","abc","fgh","de","de","mn"]
--
lfsort :: [[a]] -> [[a]]
lfsort list = map snd $ 
			  sortBy (lenFreqSort (lenFreq list)) (lenList list)

lenList :: [[a]] -> [(Int, [a])]
lenList = map (\xs -> (length xs, xs))

lenFreq :: [[a]] -> [(Int, Int)]
lenFreq = map (\xs@(x:_) -> (x, length xs)) . group . sort . map length

lenFreqSort :: [(Int, Int)] -> (Int, [a]) -> (Int, [a]) -> Ordering
lenFreqSort lenFreqDict (xlen, _) (ylen, _) =
			let xfreq = fromMaybe (-1) $ lookup xlen lenFreqDict 
			    yfreq = fromMaybe (-1) $ lookup ylen lenFreqDict
			in  xfreq `compare` yfreq
