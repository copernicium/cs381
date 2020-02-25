module Lang where -- TODO name language

-- Abstract Syntax

type Var = String

data Expr
   = LitI Int
   | LitB Bool
   | LitS String
   | Add Expr Expr
   | Declare Var Expr -- TODO figure out type system and how to track variables
   | Bind Var Expr
   | Ref Var
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | IfElse Expr Stmt Stmt --conditional expressions
   | While Expr [Stmt]
   | Begin [Stmt]
  deriving (Eq,Show)

defaultI :: Expr
defaultI = LitI 0

-- Good Examples

-- int x = 0
ex1 :: Stmt
ex1 = undefined -- TODO
