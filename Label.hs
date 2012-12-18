{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Label where

class (Show a) => Label a where
    label :: a -> String

instance Label [Char] where
    label str = str

instance Label Char where
    label c = [c]

-- Default case
instance Show a => Label a where
    label x = show x