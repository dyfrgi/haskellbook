module Vigenere where

import Data.Char (ord, chr)

enc26 :: Char -> Int
enc26 a = (ord a) - (ord 'A')

dec26 :: Int -> Char
dec26 a = chr $ (mod a 26) + ord 'A' -- we'll just do the mod here

encLetter :: Char -> Char -> Char
encLetter a b = dec26 (enc26 a + enc26 b)

vig :: String -> String -> String
vig key message =
    go (cycle key) message
    where 
        go :: String -> String -> String
        go _ [] = []    -- base case, out of message!
        go (k:ks) (m:ms)
            | m == ' '  = ' '           : go (k:ks) ms -- don't use key bytes on ' '
            | otherwise = encLetter k m : go ks   ms   -- encode everything else

