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

binop :: Binop -> Value -> Value -> Value
binop Add (VInt a) (VInt b) = VInt (a+b)
binop Sub (VInt a) (VInt b) = VInt (a-b)
binop Mul (VInt a) (VInt b) = VInt (a*b)
binop Eql (VInt a) (VInt b) = VBool (a==b)

binop Add (VBool a) (VBool b) = VBool (a || b)
binop Sub (VBool a) (VBool b) = VBool ((a && not b) || (not a && b))
binop Mul (VBool a) (VBool b) = VBool (a && b)
binop Eql (VBool a) (VBool b) = VBool (a==b)
 

eval :: TermEnv -> Expr -> Value
eval env (Lit (LInt k)) = VInt k
eval env (Lit (LBool k)) = VBool k

eval env (Var x) = 
  case Map.lookup x env of
    Just v -> v
    Nothing -> trace (show (x, env)) undefined

eval env (Op op a b) =
  let a' = eval env a
      b' = eval env b
  in binop op a' b'

eval env (Lam x body) = 
  VClosure x body env

eval env (App fun arg) = 
  let VClosure x body clo = eval env fun
      argv = eval env arg
      nenv = Map.insert x argv clo
  in eval nenv body

eval env (If cond tr fl) =
  let (VBool c) = eval env cond
  in eval env (if c then tr else fl)

eval env (Let name value expr) = 
  let env' = Map.insert name val env 
      val = eval env' value
  in eval env' expr

evalDef :: TermEnv -> Decl -> TermEnv
evalDef env (Define name value) = env'
  where 
    val = eval env' value   
    env' = Map.insert name val env

