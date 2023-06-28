module Trace 
  ( traceShows
  , traceLines
  , traceMsg
  ) where 

import Debug.Trace 

traceShows :: Show a => [a] -> b -> b
traceShows ss = traceLines (map show ss)

traceLines :: [String] -> a -> a
traceLines ls = trace (unlines ("\n--TRACE--" : ls))

traceMsg :: Show a => String -> a -> a
traceMsg msg x = trace (msg ++ show x) x
