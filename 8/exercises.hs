module NumbersToWords where

import Data.List (intersperse)

digits :: Integral a => a -> [a]
digits a
    | a < 10    = [rem a 10]
    | otherwise = (digits $ quot a 10) ++ [rem a 10]

digitToWord :: Integral a => a -> String
digitToWord d
    | d == 1 = "one"
    | d == 2 = "two"
    | d == 3 = "three"
    | d == 4 = "four"
    | d == 5 = "five"
    | d == 6 = "six"
    | d == 7 = "seven"
    | d == 8 = "eight"
    | d == 9 = "nine"
    | otherwise = "?"

wordNumber :: Integral a => a -> String
wordNumber = 
    concat .
    intersperse "-" .
    map digitToWord .
    digits
