#!/usr/bin/env stack
--stack --install-ghc --resolver lts-7.7 runghc

{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import System.Environment
import Data.Monoid

powerSet :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerSet set
  | Set.null set = Set.singleton Set.empty
  | otherwise = Set.foldr' Set.insert restPowerSet (Set.map (Set.insert (Set.findMin set)) restPowerSet)
  where
    restPowerSet = powerSet (Set.deleteMin set)


powerSet' :: Ord a => Set.Set a -> Set.Set (Set.Set a)
powerSet' set
  | Set.null set = Set.singleton Set.empty
  | otherwise =
    Set.union (powerSet (Set.deleteMin set)) $
    Set.map (Set.insert (Set.findMin set)) (powerSet (Set.deleteMin set))

powerSetLs [] = [[]]
powerSetLs (x:xs) = powerSetLs xs ++ map (x:) (powerSetLs xs)


powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss <> map (x:) xss
  where xss = powerset xs

(/\/)        :: [a] -> [a] -> [a]
[]     /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)



main :: IO ()
main = do
  [x] <- getArgs
  let xLen = read x
  print $ length $ powerSet $ Set.fromList [0..xLen]
