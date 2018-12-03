module FairDist where

import Data.Char (digitToInt)
import Test.QuickCheck

roundUpCh :: String -> Int
roundUpCh [x] = 9
roundUpCh (x:xs) = (digitToInt x + 1) * 10 ^ (length xs) - 1

roundUp :: Int -> Int
roundUp n = roundUpCh $ show n

fairDistN :: Int -> [Int] -> [Int]
fairDistN _ [] = []
fairDistN n xs@(x:xs') = 
  let sm = fromIntegral $ sum xs
      x' = round $ fromIntegral (x * n) / sm 
  in (x' : fairDistN (n - x') xs')

fairDist :: [Int] -> [Int]
fairDist xs = fairDistN (roundUp $ sum xs) xs

deltaFairDist :: [Int] -> Int
deltaFairDist xs =
  let n = roundUp $ sum xs
      n' = sum $ fairDist xs
  in (n' - n)

genPos :: Gen Int
genPos = abs `fmap` (arbitrary :: Gen Int) `suchThat` (> 0)

genListOfPos :: Gen [Int]
genListOfPos = listOf1 genPos

testFairDist =  quickCheck $ forAll genListOfPos $ \xs -> deltaFairDist xs == 0
