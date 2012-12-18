---
--- http://www.haskell.org/haskellwiki/99_questions/1_to_10
--- Lists
---

module Questions1 where

import Data.List (group)

-- Problem 1 
-- (*) Find the last element of a list.
-- myLast [1,2,3,4] == 4
-- myLast ['x','y','z'] == 'z'
--
myLast :: [a] -> a
myLast []     = error "Empty list"
myLast [x]    = x
myLast (_:xs) = myLast xs

-- Problem 2 
-- (*) Find the last but one element of a list. 
-- myButLast [1,2,3,4] == 3
-- myButLast ['a'..'z'] == 'y'
--
myButLast :: [a] -> a
myButLast []       = error "Empty list"
myButLast [_]      = error "Empty one element list"
myButLast (x:_:[]) = x
myButLast (_:xs)   = myButLast xs

-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- elementAt [1,2,3] 2 == 2
-- elementAt "haskell" 5 == 'e'
--
elementAt :: [a] -> Int -> a
elementAt _ n 
    | n <= 0    = error "Invalid index"
elementAt [] n 
    | n >= 1    = error"Too big index"
    | otherwise = error "Emtpy list"
elementAt (x:xs) n
    | n == 1    = x
    | otherwise = elementAt xs (n - 1)

-- Problem 4
-- (*) Find the number of elements of a list.
-- myLength [123, 456, 789] == 3
-- myLength "Hello, world!" == 13
--
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldl (\acc _ -> 1 + acc) 0

-- Problem 5
-- (*) Reverse a list.
-- reverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"
-- reverse [1,2,3,4] == [4,3,2,1]
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- isPalindrome [1,2,3] == False
-- isPalindrome "madamimadam" == True
-- isPalindrome [1,2,4,8,16,8,4,2,1] == True
-- 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- flatten (Elem 5) == [5]
-- flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
-- flatten (List []) == []
--
data List a = Elem a
            | List [List a]
              deriving (Show)

flatten :: List a -> [a]
flatten xs = flatten' xs []
    where flatten' (Elem y) acc = y : acc
          flatten' (List []) acc = acc
          flatten' (List (y:ys)) acc = flatten' y acc ++ (foldr flatten' acc ys)

flatten' :: List a -> [a]
flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = flatten' x ++ flatten (List xs)

-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
-- compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"] == ["a","b","c","a","d","e"]
--
compress :: (Eq a) => [a] -> [a]
compress xs = compress' xs []
    where compress' [] acc = acc
          compress' (y:ys) [] = compress' ys [y]
          compress' (y:ys) acc = if y == last acc
                                 then compress' ys acc
                                 else compress' ys (acc ++ [y])
                                       
compress' :: (Eq a) => [a] -> [a]
compress' (x:ys@(y:_))
    | x == y    = compress ys
    | otherwise = x : compress ys
compress' ys = ys

compress'' :: (Eq a) => [a] -> [a]
compress'' []  = []
compress'' (x:xs) = [x] ++ (compress (dropWhile (==x) xs))

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa","b","cc","aa","d","eeee"]
--
pack :: (Eq a) => [a] -> [[a]]
pack xs = pack' xs []
    where pack' [] acc = acc
          pack' (y:ys) [] = pack' ys [[y]]
          pack' (y:ys) acc = if y == (head $ last acc)
                             then pack' ys (init acc ++ [y : last acc])
                             else pack' ys (acc ++ [[y]])
                             
pack' xs = group xs

pack'' [] = []
pack'' (x:xs) = (x : (takeWhile (==x) xs)) : (pack'' (dropWhile (==x) xs))

-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
-- encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack
