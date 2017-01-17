#!/usr/bin/env stack
-- stack --resolver 7.16 runhaskell --package QuickCheck
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List

prop_RevRev :: Eq a => [a] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs


prop_PrefixSuffix :: Eq a => [a] -> Int -> Bool
prop_PrefixSuffix xs n = isPrefixOf prefix xs &&
                         isSuffixOf (reverse prefix) (reverse xs)
  where prefix = take n xs


prop_sqrt :: NonNegative Double -> Bool
prop_sqrt (NonNegative x)
  | x == 0 || x == 1 = sqrtX == x
  | x < 1 = sqrtX > 0 && sqrtX > x
  | x > 1 = sqrtX > 0 && sqrtX < x
  where
    sqrtX = sqrt x



prop_Index_v1 :: [Integer] -> Int -> Bool
prop_Index_v1 xs n = xs !! n == head (drop n xs)

prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = xs !! n == head (drop n xs)



prop_Index_v3 :: (NonEmptyList Integer) -> NonNegative Int -> Property
prop_Index_v3 (NonEmpty xs) (NonNegative n) = n < length xs ==> xs !! n == head (drop n xs)



prop_Index_v4 :: (NonEmptyList Integer) -> Property
prop_Index_v4 (NonEmpty xs) =
  forAll (choose (0, length xs-1)) $ \n -> xs !! n == head (drop n xs)



-- prop_PrimeMult :: (Positive Integer) -> (Positive Integer) -> Property
-- prop_PrimeMult (Positive i) (Positive j) = 



prop_Bogus :: Int -> Property
prop_Bogus n = n == 17 ==> True

prop_Bogus2 :: Property
prop_Bogus2 = forAll (return 17) $ \ n -> n == 17



data WithIndex a = WithIndex Int [a]


instance Arbitrary a => Arbitrary (WithIndex a) where
  arbitrary = do
    NonEmpty xs <- arbitrary
    NonNegative n <- arbitrary
    return $ WithIndex (n `mod` length xs) xs



main :: IO ()
main = quickCheck prop_Index_v3
