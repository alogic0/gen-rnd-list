module GenLists (rndPermute) where

import Data.List
import System.Random
import Control.Monad.State

rndPermute g n =
  let initList = [1 .. n]
  in evalState (foldM fProc [] (reverse initList)) (g, initList) 

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

