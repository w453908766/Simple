module Eval where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

import Debug.Trace

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"

type TermEnv = Map.Map String Value

binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

eval :: TermEnv -> Expr -> Value
eval env (Lit (LInt k)) = VInt k
eval env (Lit (LBool k)) = VBool k

eval env (Var x) = 
  case Map.lookup x env of
    Just v -> v
    Nothing -> trace (show (x, env)) undefined

eval env (Op op a b) =
  let VInt a' = eval env a
      VInt b' = eval env b
  in (binop op) a' b'

eval env (Lam x body) = 
  VClosure x body env

eval env (App fun arg) = 
  let VClosure x body clo = eval env fun
      argv = eval env arg
      nenv = Map.insert x argv clo
  in eval nenv body

eval env (Let x e body) =
  let e' = eval env e
      nenv = Map.insert x e' env
  in eval nenv body

eval env (If cond tr fl) =
  case eval env cond of
    VBool True  -> eval env tr
    VBool False -> eval env fl

evalDef :: TermEnv -> Decl -> TermEnv
evalDef env (Define name value) = env'
  where 
    val = eval env' value   
    env' = Map.insert name val env


