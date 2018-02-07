module MaybeLib where

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes (Just x:xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs
catMaybes [] = []

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe (Nothing:_) = Nothing
-- (Just . (x:)) is a slightly weird function, but makes sense
-- Prepend the value then raise back into Maybe
flipMaybe ((Just x):xs) = mayybee Nothing (Just . (x:)) $ flipMaybe xs
flipMaybe [] = Just []
