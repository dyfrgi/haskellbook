module StringProcessing where

notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise = Just s

-- copied from 11/capitalizewords.hs
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f s  = case dropWhile f s of                  -- find first non-split-elem
                    [] -> []                            -- if none, we're done
                    s' -> w : splitWith f s''           -- word : rest of list
                        where (w, s'') = break f s'     -- (takeUntil f s', dropUntil f s') but better, from Prelude

splitOnSpace :: String -> [String]
splitOnSpace = splitWith (\c -> c == ' ')

nothingToSomething :: Maybe String -> String
nothingToSomething (Just a) = a
nothingToSomething Nothing = "a"

replaceThe :: String -> String
replaceThe = unwords . fmap nothingToSomething . fmap notThe . splitOnSpace

isVowel :: Char -> Bool
isVowel x = elem x "aeiou"

startsWithVowel :: String -> Bool
startsWithVowel (s:_) = isVowel s

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go (splitOnSpace s)
    where go :: [String] -> Integer
          go (w1:w2:ws)
            | w1 == "the" && startsWithVowel w2 = 1 + go ws
            | otherwise                         = go $ w2:ws
          go _ = 0 -- one base case! _ to the rescue

countVowels :: String -> Int
countVowels = length . filter isVowel
