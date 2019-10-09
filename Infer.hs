
{-# LANGUAGE FlexibleInstances #-}

module Infer (Env, inferTop) where

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Debug.Trace

import Type
import Syntax
import Solve

type Constraint = (Type, Type)
type Env = Map.Map String Scheme

type InferState = (Env, [Constraint], [String])
type Infer a = StateT InferState (Except TypeError) a

----------------------------------------------------


instantiate :: Scheme -> Infer Type
instantiate (Forall xs ty) = do
  let ts = fmap TVar xs
  ts' <- traverse (const newTVar) xs
  let tymap = Map.fromList (zip ts ts')
  let ty' = apply tymap ty
  return ty'



lookupEnv :: String -> Infer Type
lookupEnv x = do
  (env, _, _) <- get
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> instantiate s

 

putEnv :: Env -> Infer ()
putEnv env = do
  (_, cs, letters) <- get
  put (env, cs, letters)

putConstraint :: (Type, Type) -> Infer ()
putConstraint c = do
  (env, cs, letters) <- get
  put (env, c:cs, letters)


newTVar :: Infer Type
newTVar = do
  (env, cs, xss) <- get
  let (x:xs) = xss
  put (env, cs, xs)
  return $ TVar x


ops :: Binop -> Type
ops Eql = typeBool
ops _ = typeInt


infer :: Expr -> Infer Type
infer (Lit (LInt _)) = return typeInt
infer (Lit (LBool _)) = return typeBool

infer (Var name) = lookupEnv name

infer (Op binop a b) = do
  a' <- infer a
  b' <- infer b
  putConstraint (a', typeInt)
  putConstraint (b', typeInt)
  return $ ops binop

infer (If cond tr fl) = do
  cond' <- infer cond
  tr' <- infer tr
  fl' <- infer fl
  putConstraint (cond', typeBool)
  putConstraint (tr', fl')
  return tr'

infer (App e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- newTVar

  putConstraint (t1, TArr t2 tv)
  return tv

infer (Lam name expr) = do
  (vt, rt) <- withVar name expr
  return (TArr vt rt)

inferDefine :: Decl -> Infer Type
inferDefine (Define name expr) = do
  (vt, rt) <- withVar name expr
  putConstraint (vt, rt)
  return rt

withVar :: String -> Expr -> Infer (Type, Type)
withVar name expr = do
  vt <- newTVar
  (env, _, _) <- get
  let env' = Map.insert name (Forall [] vt) env
  putEnv env'
  rt <- infer expr
  putEnv env
  return (vt, rt)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

closeOver :: Type -> Scheme
closeOver ty = Forall (Set.toList $ ftv ty) ty
  
runInfer :: Env -> Expr -> Either TypeError (Type, InferState)
runInfer env expr = do
  let state = (env, [], letters)
  runExcept $ runStateT (infer expr) state


inferExpr :: Env -> Expr -> Either TypeError Scheme
inferExpr env expr = do
  (ty, (_,cs,_)) <- runInfer env expr
  tymap <- solveType cs
  return $ closeOver $ apply tymap ty

inferDecl :: Env -> Decl -> Either TypeError Scheme
inferDecl env decl = do
  let state = (env, [], letters)
  (ty, (_,cs,_)) <- runExcept $ runStateT (inferDefine decl) state
  tymap <- solveType cs
  return $ closeOver $ apply tymap ty
  

inferTop :: Env -> [Decl] -> Either TypeError Env
inferTop env [] = Right env
inferTop env (decl@(Define name ex):xs) = 
  case inferDecl env decl of
    Left err -> Left err
    Right ty -> inferTop (Map.insert name ty env) xs


