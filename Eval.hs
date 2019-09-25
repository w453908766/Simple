module Eval where

import Syntax

import Control.Monad.Identity
import qualified Data.Map as Map

data Value
  = VInt Integer
  | VBool Bool
  | VClosure String Expr TermEnv

type TermEnv = Map.Map String Value

emptyTmenv :: TermEnv
emptyTmenv = Map.empty

instance Show Value where
  show (VInt n) = show n
  show (VBool n) = show n
  show VClosure{} = "<<closure>>"


binop :: Binop -> Integer -> Integer -> Value
binop Add a b = VInt $ a + b
binop Mul a b = VInt $ a * b
binop Sub a b = VInt $ a - b
binop Eql a b = VBool $ a == b

eval :: TermEnv -> Expr -> Value
eval env expr = case expr of
  Lit (LInt k)  -> VInt k
  Lit (LBool k) -> VBool k

  Var x -> let Just v = Map.lookup x env in v

  Op op a b ->
    let VInt a' = eval env a
        VInt b' = eval env b
    in (binop op) a' b'

  Lam x body -> VClosure x body env

  App fun arg ->
    let VClosure x body clo = eval env fun
        argv = eval env arg
        nenv = Map.insert x argv clo
    in eval nenv body

  Let x e body ->
    let e' = eval env e
        nenv = Map.insert x e' env
    in eval nenv body

  If cond tr fl ->
    let VBool br = eval env cond in
    if br == True
    then eval env tr
    else eval env fl

  Fix e ->
    eval env (App e (Fix e))


runEval :: TermEnv -> String -> Expr -> (Value, TermEnv)
runEval env nm ex =
  let res = eval env ex in
  (res, Map.insert nm res env)
