module Debug where 

import Debug.Trace

traceList :: (Show b) => [b] -> a -> a
traceList xs y = trace msg y
    where msg = foldl (\acc x -> acc ++ show x ++ " ") "" xs

  
{-# LANGUAGE CPP #-}  
--parseTree all@(_)    = trace ("\n" ++ __FILE__ ++ ":" ++ show __LINE__ ++ " Unparsed: " ++ all) undefined