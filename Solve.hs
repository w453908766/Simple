
{-# LANGUAGE FlexibleInstances #-}

module Solve (solveType, Substitutable(..)) where

import Control.Monad.ST
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.UnionFind.ST

import Debug.Trace

import Type

type PointMap s a = Map.Map a (Point s a)
type Pair a = (a,a)

type TypeMap = Map.Map Type Type

class Substitutable a where
  apply :: TypeMap -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply map t@(TCon _) = t
  apply map (TArr a b) = TArr (apply map a) (apply map b)
  apply map t@(TVar _) = 
    case Map.lookup t map of
      Nothing -> t
      Just x -> apply map x

  ftv TCon{}         = Set.empty
  ftv (TVar a)       = Set.singleton a
  ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2


instance (Functor t, Foldable t, Substitutable a) => Substitutable (t a) where
  apply = fmap . apply
  ftv   = foldr (Set.union . ftv) Set.empty


getMap :: Ord a => Map.Map a b -> a -> ST s b -> ST s (b, Map.Map a b)
getMap map key pvalue = do
  case Map.lookup key map of
    Just v -> return (v, map)
    Nothing -> do
      value <- pvalue
      return (value, Map.insert key value map)



unionType :: PointMap s Type -> Pair Type -> ST s (PointMap s Type)
unionType map0 (x, y) = do
  (px, map1) <- getMap map0 x (fresh x)
  x' <- descriptor px

  (py, map2) <- getMap map1 y (fresh y)
  y' <- descriptor py

  case (x', y') of
    (TVar _, _) -> do
      union px py
      return map2
    (_, TVar _) -> do
      union py px
      return map2

    (TArr a b, TArr c d) -> do
      map3 <- unionType map2 (a,c) 
      map4 <- unionType map3 (b,d) 

      return map4
    _ -> return map2

     
    
  
filterItem :: Type -> a -> Bool
filterItem (TVar _) _ = True
filterItem _ _ = False

solveType :: [Pair Type] -> Map.Map Type Type
solveType cs = runST $ do 
  map <- foldM unionType Map.empty cs
  let map1 = Map.filterWithKey filterItem map
  tymap <- traverse descriptor map1
  return $ apply tymap tymap


{- 
p :: Type -> Type -> ST s Type
p (TVar _) y = return y
p x (TVar _) = return x
p x@(TCon a) (TCon b) 
 |a==b = return x

sortCS :: [Pair Type] -> [Pair Type]
sortCS [] = []
sortCS ((x,y):cs) =
  let rest = sortCS cs in
  case (x,y) of
    (TVar _, _) -> ((x,y):rest)
    (_, TVar _) -> ((y,x):rest)
    (TCon _, TCon _) -> rest
    (TArr a b, TArr c d) -> sortCS [(a,c),(b,d)] ++ rest
-}
