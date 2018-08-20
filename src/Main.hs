module Main where

import Data.List
import System.Random
import Control.Monad.State

main :: IO ()
main = do
  g <- getStdGen
  let initList = [1 .. 7]
  let resLst = evalState (foldM fProc [] (reverse initList)) (g, initList) 
  putStrLn $ show resLst

fProc :: [Int] -> Int -> State (StdGen, [Int]) [Int]
fProc accLst n = do
  (g, lst) <- get
  let (idx, g1) = randomR (1, n) g
  let nextElem = lst !! (idx - 1)
  let lst1 = delete nextElem lst
  put (g1, lst1)
  return (nextElem : accLst)

-- evalState (mkStdGen n,  
--randLst lst = 
--  foldM fProc [] (reverse lst)

