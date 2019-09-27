
module Solve (solveType) where

import Control.Monad.ST
import Control.Monad
import Data.Map as Map
import Data.UnionFind.ST

import Debug.Trace

import Type

type PointMap s a = Map a (Point s a)
type Pair a = (a,a)

getMap :: Ord a => Map a b -> a -> ST s b -> ST s (b, Map a b)
getMap map key pvalue = do
  case Map.lookup key map of
    Just v -> return (v, map)
    Nothing -> do
      value <- pvalue
      return (value, insert key value map)


solve :: Ord a => (PointMap s a -> Pair a -> ST s (PointMap s a)) -> [Pair a] -> ST s (Map a a)
solve union cs = do
  map <- foldM union Map.empty cs
  traverse descriptor map


unionType :: PointMap s Type -> Pair Type -> ST s (PointMap s Type)
unionType map0 (x@(TArr a b), (TArr c d)) = do
  map1 <- unionType map0 (a,c)
  map2 <- unionType map1 (b,d)
  return map2

unionType map0 (x, y) = do
  (px, map1) <- getMap map0 x (fresh x)
  (py, map2) <- getMap map1 y (fresh y)
  union' px py p
  return map2
  
p :: Type -> Type -> ST s Type
p (TVar _) y = return y
p x (TVar _) = return x
p x@(TCon a) (TCon b) 
 |a==b = return x


solveType :: [Pair Type] -> Map Type Type
solveType cs = runST (solve unionType cs)


