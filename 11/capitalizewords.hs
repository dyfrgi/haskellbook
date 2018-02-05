module Capitalize where

import Data.Char (toUpper)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f s  = case dropWhile f s of                  -- find first non-split-elem
                    [] -> []                            -- if none, we're done
                    s' -> w : splitWith f s''           -- word : rest of list
                        where (w, s'') = break f s'     -- (takeUntil f s', dropUntil f s') but better, from Prelude

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\w@(f:fs) -> (w, toUpper f : fs)) 
                    $ splitWith (' ' ==) s
