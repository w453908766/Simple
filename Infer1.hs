
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Type
import Syntax
import Solve

type Constraint =(Type, Type)
type Env = Map.Map String Scheme
type TypeMap = Map.Map Type Type

type InferState = (Env, [Constraint], [String])
type Infer a = StateT InferState (Except TypeError) a


data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving (Show, Eq)

class Substitutable a where
  apply :: TypeMap -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply map t@(TCon _) = t
  apply map (TArr a b) = TArr (apply map a) (apply map b)
  apply map t@(TVar a) = Map.findWithDefault t t map
  

----------------------------------------------------

lookupEnv :: String -> Infer Type
lookupEnv x = do
  (env, _, _) <- get
  case Map.lookup x env of
      Nothing   -> throwError $ UnboundVariable x
      Just s    -> instantiate s

instantiate :: Scheme -> Infer Type
instantiate (Forall vs0 ty) = do
  let vs = map TVar vs0
  vs' <- traverse (const newTVar) vs
  let tyenv = Map.fromList $ zip vs vs'
  return $ apply tyenv ty
  

putEnv :: Env -> Infer ()
putEnv env = do
  (_, cs, letters) <- get
  put (env, cs, letters)

putConstraint :: (Type, Type) -> Infer ()
putConstraint c = do
  (env, cs, letters) <- get
  put (env, c:cs, letters)

{-
putTyEnv :: (Type, Type) -> Infer ()
putTyEnv (TVar t0, t1) = do
  (env, cs, letters) <- get
  put (env, Map.insert t0 t1 tyenv, cs, letters)
-}



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

infer (Lam name expr) = do
  vt <- newTVar
  (env, _, _) <- get
  let env' = Map.insert name (Forall [] vt) env
  putEnv env'
  rt <- infer expr
  putEnv env
  return (TArr vt rt)

infer (App e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  tv <- newTVar
  putConstraint (t1, TArr t2 tv)
  return tv


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

  
runInfer :: Env -> Expr -> Either TypeError (Type, InferState)
runInfer env expr = do
  let state = (env, [], letters)
  runExcept $ runStateT (infer expr) state

-----------------------------------------------------

  
f = Lam "x" $ Op Add (Var "x") (Lit $ LInt 1)
  
f0 = Lam "p" $ Lam "x" $ If (Var "p") (Var "x") (Lit $ LInt 5)
-- f0 = \p -> \x -> if p then x else 5

Right (ty, (env, cs, _)) = runInfer Map.empty f0
   
tymap = solveType cs

t = apply tymap ty
  

   
