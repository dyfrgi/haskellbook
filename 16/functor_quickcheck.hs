module FunctorQuickcheck where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) =>
                       f a
                    -> Bool
functorIdentity f =
    fmap id f == f

functorCompose' :: (Functor f, Eq (f c)) =>
                       f a
                    -> Fun a b
                    -> Fun b c
                    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

fiListInt :: [Int] -> Bool
fiListInt = functorIdentity

fcListInt :: [Int] -> Fun Int Float -> Fun Float Char -> Bool
fcListInt = functorCompose'

newtype Identity a = Identity a
data Pair a = Pair a a
    deriving (Show, Eq)
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
    deriving (Show, Eq)
data Four a b c d = Four a b c d
data Four' a b = Four' a a a b

type FcInt f = (f Int) -> Fun Int Float -> Fun Float Char -> Bool

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a =>
         Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

main :: IO ()
main = do
    quickCheck fiListInt
    quickCheck fcListInt
    quickCheck (functorCompose' :: FcInt [])
    quickCheck (functorCompose' :: FcInt Pair)
    quickCheck (functorCompose' :: FcInt (Three' String))
    verboseCheck (functorCompose' :: FcInt (Three' String))
