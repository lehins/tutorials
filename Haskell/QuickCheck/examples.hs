#!/usr/bin/env stack
-- stack runhaskell --resolver 2.22 --package QuickCheck
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
prop_Index_v1 xs n = (xs !! n) == head (drop n xs)

prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = (xs !! n) == head (drop n xs)



prop_Index_v3 :: (NonEmptyList Integer) -> NonNegative Int -> Property
prop_Index_v3 (NonEmpty xs) (NonNegative n) = n < length xs ==> (xs !! n) == head (drop n xs)





split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

unsplit :: Char -> [String] -> String
unsplit c = concat . intersperse [c]


prop_split_inv xs = forAll (elements xs) $ \c -> (unsplit c (split c xs) == xs)


main :: IO ()
main = quickCheck prop_Index_v3
