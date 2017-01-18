#!/usr/bin/env stack
-- stack --resolver lts-7.16 runhaskell --package QuickCheck --package hspec --package primes
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Test.Hspec
import Test.QuickCheck
import Data.List
import Data.Numbers.Primes

prop_RevRevPoly :: Eq a => [a] -> Bool
prop_RevRevPoly xs = reverse (reverse xs) == xs

prop_RevRev :: [Char] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

prop_RevApp :: [Int] -> [Int] -> Bool
prop_RevApp xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs


prop_PrefixSuffix :: [Int] -> Int -> Bool
prop_PrefixSuffix xs n = isPrefixOf prefix xs &&
                         isSuffixOf (reverse prefix) (reverse xs)
  where prefix = take n xs


prop_Sqrt :: Double -> Bool
prop_Sqrt x
  | x < 0            = isNaN sqrtX
  | x == 0 || x == 1 = sqrtX == x
  | x < 1            = sqrtX > x
  | x > 1            = sqrtX > 0 && sqrtX < x
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



-- rpm :: Integral a => a -> a -> a
-- rpm x y
--   | x == 0 || y == 0 = 0
--   | x < 0  || y < 0  = error "Cannot operate on negative numbers"
--   | otherwise        = go x y 0
--   where
--     go 1 b acc = b + acc
--     go a b acc = go (a `div` 2) (b + b) (if a `mod` 2 == 1 then (acc + b) else acc)


-- prop_RPM_NegX :: Positive Int -> Int -> Property
-- prop_RPM_NegX (Positive x) y = expectFailure (rpm (-x) y == (-x) * y)

-- prop_RPM_NegX :: Positive Int -> Int -> Property
-- prop_RPM_NegX (Positive x) y = expectFailure (rpm (-x) y == (-x) * y)

-- prop_RPM_neg :: Int -> Int -> Property
-- prop_RPM x y | x < 0  || y < 0 = expectFailure (rpm x y == x * y)
--              | otherwise = within 500000 (rpm x y == x * y)



prop_PrimeFactors :: (Positive Int) -> Bool
prop_PrimeFactors (Positive n) = isPrime n || all isPrime (primeFactors n)


prop_PrimeSum_v0 :: (Positive Int) -> (Positive Int) -> Property
prop_PrimeSum_v0 (Positive p) (Positive q) =
  classify precondition "precond" $
  precondition ==>
  even (p + q)
  where
    precondition = p > 2 && q > 2 && isPrime p && isPrime q


prop_PrimeSum_v1 :: Int -> Int -> Property
prop_PrimeSum_v1 p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==> even (p + q)


prop_PrimeSum_v1' :: Int -> Int -> Property
prop_PrimeSum_v1' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  classify (p < 20 && q < 20) "trivial" $ even (p + q)


prop_PrimeSum_v2 :: (Positive (Large Int)) -> (Positive (Large Int)) -> Property
prop_PrimeSum_v2 (Positive (Large p)) (Positive (Large q)) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p, q) else (q, p)) $ even (p + q)


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
prop_MapMap f g xs = map g (map f xs) == map (g . f) xs



-- Proper way to generate functions.

newtype TestFunction a b = TestFunction (a -> b)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (TestFunction a b) where
  arbitrary = TestFunction <$> arbitrary

instance Show (TestFunction Int Char) where
  show _ = "Function: (Int -> Char)"

instance Show (TestFunction Char (Maybe Double)) where
  show _ = "Function: (Char -> Maybe Double)"


prop_MapMap' :: TestFunction Int Char -> TestFunction Char (Maybe Double) -> [Int] -> Bool
prop_MapMap' (TestFunction f) (TestFunction g) xs = map g (map f xs) == map (g . f) xs


main :: IO ()
main = hspec $ do
  describe "Reverse Properties" $
    do it "prop_RevRev" $ property prop_RevRev
       it "prop_RevApp" $ property prop_RevApp
       it "prop_PrefixSuffix" $ property prop_PrefixSuffix
  describe "Number Properties" $
    do it "prop_Sqrt" $ property prop_Sqrt
  describe "Index Properties" $
    do it "prop_Index_v3" $ property prop_Index_v3
       it "prop_Index_v4" $ property prop_Index_v4
       it "negativeIndex" $ shouldThrow (return $! ([1,2,3] !! (-1))) anyException
       it "emptyIndex" $ shouldThrow (return $! ([] !! 0)) anyException
       it "emptyIndex" $ shouldBe (([1,2,3] !! 1)) 2
  describe "Prime Numbers" $
    do it "prop_PrimeFactors" $ property prop_PrimeFactors
       it "prop_PrimeSum_v3" $ property prop_PrimeSum_v3
       it "prop_PrimeSum_v4" $ property prop_PrimeSum_v4
  describe "High Order" $
    do it "prop_MapMap" $ property prop_MapMap
       

