---
--- http://www.haskell.org/haskellwiki/99_questions/11_to_20
--- Lists
---

module Questions2 where

import Questions1
import Data.List (splitAt)

-- Problem 11
-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
-- encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--
data Element a = Single a
               | Multiple Int a
                 deriving (Show)

encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified = map process . pack
    where process xs = if l == 1 then Single x else Multiple l x
            where l = length xs
                  x = head xs
          
-- Problem 12
-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
-- decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee"
--
decodeModified :: [Element a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs)     = x : decodeModified xs
decodeModified ((Multiple l x):xs) = (replicate l x) ++ decodeModified xs

decodeModified' :: [Element a] -> [a]
decodeModified' xs = foldl (\acc x -> case x of
                                        Single x -> acc ++ [x]
                                        Multiple l x -> acc ++ replicate l x) [] xs

-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. 
-- I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. 
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
-- encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--
encodeDirect :: (Eq a) => [a] -> [Element a]
encodeDirect [] = []
encodeDirect (x:xs) = (buildElement (buildGroup x xs)) : (encodeDirect (dropWhile (==x) xs))
    where buildGroup y ys = y : (takeWhile (==y) ys)
          buildElement zs = if l == 1 then Single z else Multiple l z
            where l = length zs
                  z = head zs

-- Problem 14
-- (*) Duplicate the elements of a list.
-- dupli [1, 2, 3] == [1,1,2,2,3,3]
--
dupli :: [a] -> [a]
dupli = concat . map (replicate 2)

-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
-- repli "abc" 3 == "aaabbbccc"
--
repli :: [a] -> Int -> [a]
repli xs n = concat $ map (replicate n) xs

-- Problem 16
-- (**) Drop every N'th element from a list.
-- dropEvery "abcdefghik" 3 == "abdeghk"
-- 
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = fst $ foldl (\(acc, i) y ->
                                if i `mod` n == 0
                                then (acc, i + 1)
                                else (acc ++ [y], i + 1)) ([], 1) xs

-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
-- split "abcdefghik" 3 == ("abc", "defghik")
--
split :: [a] -> Int -> ([a], [a])
split xs n = foldl (\(l, r) x -> 
                       if length l < n
                       then (l ++ [x], r)
                       else (l, r ++ [x])) ([], []) xs

-- Problem 18
-- (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included).
-- Start counting the elements with 1.
-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"
-- 
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop (i - 1) xs

-- Problem 19
-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc"
-- rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"
-- 
rotate :: [a] -> Int -> [a]
rotate xs n
    | null xs = xs
    | n > 0 = rotateLeft xs n
    | n < 0 = rotateRight xs n
    | otherwise = xs
    where rotateLeft ys 0 = ys
          rotateLeft (y:ys) n = rotateLeft (ys ++ [y]) (n - 1)
          rotateRight ys 0 = ys
          rotateRight ys n = rotateRight (last ys : init ys) (n + 1)

-- Problem 20
-- (*) Remove the K'th element from a list.
-- removeAt 1 "abcd" == ('b',"acd")
-- 
removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = (elem, before ++ after)
    where elem = xs !! k
          before = take k xs
          after = drop (k + 1) xs
          
removeAt' k xs = (\(before, after) -> (head after, before ++ tail after)) $ splitAt k xs