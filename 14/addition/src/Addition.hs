module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)

-- Suggested exercise: Write multipliedBy :: (Eq a, Num a) => a -> a -> a
-- How do you handle non-integral numbers with only those types without using
-- the built-in (*) or (/) from Num? Seems boring to use those, but without
-- them it seems a lost cause.
-- Bottom is lurking. Waiting. Watching.

testAddition :: IO ()
testAddition = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is 4" $ do
            2 + 2 `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
