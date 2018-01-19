module Exercises where
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase a b s =
    case s of
        True -> a
        False -> b

foldBoolGuards :: a -> a -> Bool -> a
foldBoolGuards a b s
    | s == True = a
    | otherwise = b

tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = fst $ quotRem x 10
          d     = snd $ quotRem xLast 10
