module Lang where -- TODO name language


--Syntax of the "core" language start
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

data Math
   = LTE Expr Expr         -- less than or equal to
   = LT  Expr Expr         -- less than
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | IfElse Expr Stmt Stmt --conditional expressions
   | While Math [Stmt]     --Modify Expr to Math (David)
   | Begin [Stmt]
  deriving (Eq,Show)

defaultI :: Expr
defaultI = LitI 0

--"core" language End

--Here are some example expressions:
-- Good Examples
-- int x = 0
ex1 :: Stmt     --Expr?
ex1 = undefined -- TODO

--While
--
--   begin
--     R := 1
--     while R < 50
--       R := R + R
--   end
ex2 :: stmt
ex2 = Begin [Set (LitI 1),while (LT (Ref Var) (Lit 50)) [Set (Add (Ref Var) (Ref Var)) ]]



--Identify/define the semantic domain for this language
-- Type
--   *Int
--   *String
--   *Bool
--   *Type Error
--data domain  = I Int
--             | S String
--             | B Bool
--             | Error
data maybe a = Nothing | Just a
data domain a b c = I a | S b | B c



-- Valuation function for expressions.
