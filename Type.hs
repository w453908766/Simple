module Type where

type TVar = String

data Type
  = TVar String
  | TCon String
  | TArr Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [(Type, Type)]
  | UnificationMismatch [Type] [Type]
  deriving (Eq)



typeInt, typeBool :: Type
typeInt  = TCon "Int"
typeBool = TCon "Bool"
