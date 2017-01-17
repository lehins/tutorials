#!/usr/bin/env stack
-- stack --resolver lts-7.16 runhaskell --package QuickCheck --package primes
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List
import Data.Numbers.Primes

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



prop_Bogus :: Int -> Property
prop_Bogus n = n == 17 ==> True

prop_Bogus2 :: Property
prop_Bogus2 = forAll (return 17) $ \ n -> n == 17


prop_PrimeSum_v0 :: (Positive Int) -> (Positive Int) -> Property
prop_PrimeSum_v0 (Positive p) (Positive q) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)


prop_PrimeSum_v1 :: Int -> Int -> Property
prop_PrimeSum_v1 p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==> even (p + q)



prop_PrimeSum_v1'' :: Int -> Int -> Property
prop_PrimeSum_v1'' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)

prop_PrimeSum_v1' :: Int -> Int -> Property
prop_PrimeSum_v1' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  classify (p < 20 && q < 20) "trivial" $ even (p + q)


prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)


-- |
-- λ> quickCheck prop_PrimeMult
-- *** Gave up! Passed only 24 tests:
-- 20% (3,7)
-- 20% (3,5)
--  8% (5,5)
--  8% (3,3)
--  8% (3,13)
--  4% (7,7)

prop_PrimeSum_v3 :: Property
prop_PrimeSum_v3 =
  forAll (choose (1, 1000)) $ \ i ->
    forAll (choose (1, 1000)) $ \ j ->
      let (p, q) = (primes !! i, primes !! j) in
      collect (if p < q then (p, q) else (q, p)) $ even (p + q)


newtype Prime a = Prime a deriving Show

instance (Integral a, Arbitrary a) =>
         Arbitrary (Prime a) where
  arbitrary = do
    x <- frequency [ (10, choose (0, 1000))
                   , (5, choose (1001, 10000))
                   , (1, choose (10001, 50000))
                   ]
    return $ Prime (primes !! x)


prop_PrimeSum_v4 :: Prime Int -> Prime Int -> Property
prop_PrimeSum_v4 (Prime p) (Prime q) =
  p > 2 && q > 2 ==> classify (p < 1000 || q < 1000) "has small prime" $ even (p + q)


instance Show (Int -> Char) where
  show _ = "Function: (Int -> Char)"

instance Show (Char -> Maybe Double) where
  show _ = "Function: (Char -> Maybe Double)"


prop_MapMap :: (Int -> Char) -> (Char -> Maybe Double) -> [Int] -> Bool
prop_MapMap f g ls = map g (map f ls) == map (g . f) ls

-- prop_FTH :: (Positive Int) -> Property
-- prop_FTH (Positive n) = n > 1 ==> all isPrime $ primeFactors n

main :: IO ()
main = quickCheck prop_Index_v3
