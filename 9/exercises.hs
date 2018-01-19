module Chapter9 where

myEFT :: Enum a => a -> a -> [a]
myEFT a b = go (fromEnum a) (fromEnum b) -- map to int since Enum isn't Ord
    where go a b
            | a > b     = []
            | otherwise = (toEnum a) : go (a + 1) b -- can always add 1 to a because using int
