module MoreFunctors where

replaceWithP :: a -> Char
replaceWithP = const 'P'

data K a b =
    K a
    deriving (Show)

instance Functor (K x) where
    fmap _ (K a) = K a

ks::[K Int String]
ks = [K 3, K 8]


data LiftItOut f a =
    LiftItOut (f a)
    deriving (Show)

instance Functor f =>
         Functor (LiftItOut f) where
    fmap y (LiftItOut fa) = LiftItOut $ fmap y fa

lioList :: LiftItOut [] Int
lioList = LiftItOut [5, 7, 9]

data IgnoreOne f g a b =
    IgnoreSomething (f a) (g b)
    deriving Show

instance Functor g =>
         Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

data List a =
      Nil
    | Cons a (List a)
    deriving (Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

manualList :: List Int
manualList = Cons 5 (Cons 3 (Cons 12 Nil))

data TalkToMe a =
      Halt
    | Print String a
    | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read x)    = Read $ f . x
