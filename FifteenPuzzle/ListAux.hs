module ListAux ( 
   insertAt
 , replaceAt
 , middle
 , wordsWith
 , unique  
 ) where

-- | The 'insertAt' function takes an element, a list, and an index and inserts the element
-- into the list before the index position.
--
-- > insertAt 10 [1,2,3,4,5] 2 == [1,2,10,3,4,5]
--
insertAt :: a -> [a] -> Int -> [a]
insertAt e xs i = (\(before, after) -> before ++ (e : after)) 
                $ splitAt i xs
  
-- | The 'replaceAt' function takes an element, a list, and an index and replaces an element
-- at the index position with the given element.
--
-- > replaceAt 10 [1,2,3,4,5] 2 == [1,2,10,4,5]
--
replaceAt :: a -> [a] -> Int -> [a]
replaceAt e xs i = (\(before, after) -> before ++ (e : (tail after)))
                 $ splitAt i xs
                 
-- | Extract the middle element of a list, which must be non-empty.
middle :: [a] -> a
middle [] = errorEmptyList "middle"
middle xs = let index = floor $ (fromIntegral $ length xs) / 2
            in xs !! index
  
-- Data.List.Split  
-- cabal install split
-- >splitOn "," "my,comma,separated,list" == ["my","comma","separated","list"]

-- | 'wordsWith' breaks a string up into a list of words, which were delimited
-- when the predicate returns True.
wordsWith :: (Char -> Bool) -> String -> [String]
wordsWith p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWith p s''
                        where (w, s'') = break p s'
                              
-- | The 'unique' function takes a list and tells whether or not all elements are unique.
-- > unique "abcde" == True                              
-- > unique "abccde" = False                              
--                              
unique :: Eq a => [a] -> Bool     
unique [] = True
unique (x:xs) = if x `elem` xs
                then False   
                else unique xs
            
-- | Error message
errorEmptyList :: String -> a
errorEmptyList fun =
  error (listaux_list_str ++ fun ++ ": empty list")

listaux_list_str :: String
listaux_list_str = "ListAux."