module Subseq where

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True                      -- out of pattern
isSubseqOf _ [] = False                     -- out of document, but not out of pattern
isSubseqOf pat@(p:ps) doc@(d:ds) = case p == d of
    True  -> isSubseqOf ps ds         -- consume one elem of each
    False -> isSubseqOf pat ds        -- skip to next elem of document
