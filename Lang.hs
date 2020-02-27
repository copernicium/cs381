module Lang where -- TODO name language

import Prelude hiding (LT, EQ, GT)

--Syntax of the "core" language start
-- Abstract Syntax
type Var = String

data Expr
   = LitI Int
   | LitB Bool
   | LitS String
   | Ref Var
   | Add Expr Expr
   | Sub Expr Expr
   | Mul Expr Expr
   | LT Expr Expr
   | LTE Expr Expr
   | EQ Expr Expr
   | GTE Expr Expr
   | GT Expr Expr
   | NE Expr Expr
   | Ternary Expr Expr Expr
  deriving (Eq,Show)

data Stmt
   = Declare Var Expr
   | Bind Var Expr
   | IfElse Expr Stmt Stmt --conditional expressions
   | While Expr Stmt
   | Begin [Stmt]
  deriving (Eq,Show)

--"core" language End

--Here are some example expressions:
-- Good Examples

-- int x = 0
ex1 :: [Stmt]
ex1 = [Declare "x" (LitI 0)]

-- int x = 4
-- int y = 5
-- int z = x + y
--
ex2 :: [Stmt]
ex2 = [Declare "x" (LitI 4),
       Declare "y" (LitI 5),
       Declare "z" (Add (Ref "x") (Ref "y"))]

-- int i = 0
-- int y = 1
-- while i < 5
-- begin
--    y = y * 2
--    i = i + 1
-- end
--
ex3 :: [Stmt]
ex3 = [Declare "i" (LitI 0),
       Declare "y" (LitI 1),
       While (LT (Ref "i") (LitI 5)) (Begin [
          (Bind "y" (Mul (Ref "y") (LitI 2))),
          (Bind "i" (Add (Ref "i") (LitI 1)))
       ])]

--Identify/define the semantic domain for this language
-- Type
--   *Int
--   *String
--   *Bool
--   *Type Error
data Value 
   = I Int 
   | S String
   | B Bool
   | Error
  deriving (Eq,Show)

type Env = [(Var, Value)] -- TODO scopes

-- | Get the value of a variable
--
ref :: Var -> Env -> Value
ref _ []      = Error
ref var ((name, val) : t) = if name == var then val else ref var t

-- | Check if a variable is defined
--
find :: Var -> Env -> Bool
find var env = case ref var env of 
                 Error -> False
                 _ -> True

-- | Apply an arithmetic operator (e.g. addition) to two expressions
--
arithmeticOp :: Expr -> Expr -> Env -> (Int -> Int -> Int) -> Value
arithmeticOp a b env op = case (expr a env, expr b env) of
                            (I i, I j) -> I (op i j)
                            _ -> Error

-- | Evaluate two expressions of LitI with a relational/comparison operator
--
relationalOp :: Expr -> Expr -> Env -> (Int -> Int -> Bool) -> Value
relationalOp a b env cmp = case (expr a env, expr b env) of
                             (I i, I j) -> B (cmp i j)
                             _ -> Error

-- Evaluation function for an expression
--
expr :: Expr -> Env -> Value
expr (LitI x) env        = I x
expr (LitS x) env        = S x
expr (LitB x) env        = B x
expr (Ref var) env       = ref var env
expr (Add a b) env       = case (expr a env, expr b env) of
                             (I i, I j) -> I (i + j)
                             (S i, S j) -> S (i ++ j)
                             _ -> Error
expr (Sub a b) env       = arithmeticOp a b env (-)
expr (Mul a b) env       = arithmeticOp a b env (*)
expr (LT a b) env        = relationalOp a b env (<)
expr (LTE a b) env       = relationalOp a b env (<=)
expr (EQ a b) env        = relationalOp a b env (==)
expr (GTE a b) env       = relationalOp a b env (>=)
expr (GT a b) env        = relationalOp a b env (>)
expr (NE a b) env        = relationalOp a b env (/=)
expr (Ternary c t e) env = case (expr c env) of 
                             (B True)  -> expr t env
                             (B False) -> expr e env
                             _ -> Error

-- | Bind an existing variable to a new value
--
bind :: Var -> Value -> Env -> Maybe Env
bind _ _ []              = Nothing
bind var val ((name, val') : t) 
  | name == var = Just ((var, val) : t)
  | otherwise   = case bind var val t of 
                    Nothing -> Nothing
                    Just t -> Just ((name, val') : t)

-- | Evaluation function for a single statement
--
stmt :: Stmt -> Env -> Maybe Env
stmt (Declare var e) env = if find var env then Nothing else Just ((var, expr e env) : env)
stmt (Bind var e) env    = bind var (expr e env) env
stmt (IfElse c t e) env  = case expr c env of
                             B True  -> stmt t env
                             B False -> stmt e env
                             _ -> Nothing
stmt (While c b) env     = case expr c env of
                             B True  -> case stmt b env of
                                          Nothing   -> Nothing
                                          Just env' -> stmt (While c b) env'
                             B False -> Just env
                             _ -> Nothing
stmt (Begin b) env       = eval b env

-- | Evaluation function for a list of statements
--
eval :: [Stmt] -> Env -> Maybe Env
eval [] env = Just env
eval (h:t) env = case stmt h env of
                   Nothing   -> Nothing
                   Just env' -> eval t env'




-- Static type system

--typeOf :: Expr -> Maybe Var
--typeOf = undefined


data Type = TInt | TString | TBool | TError
     deriving (Eq,Show)

--Helper function
ref2 :: Var -> Env -> Maybe Type
ref2 _ []      = Nothing
ref2 var ((name, val) : t) | var ==name = case val of 
                                           (I v) -> Just TInt
                                           (S s) -> Just TString
                                           (B b) -> Just TError
                           | otherwise = ref2 var t
                           
--Type good case
ex4:: Expr
ex4 = (Add (LitI 5) (LitI 3))


ex6:: Expr
ex6 = (Add (LitI 5) (Ref "x"))

ex7 :: [Stmt]
ex7 = [Declare "i" (LitI 0),
       Declare "y" (LitI 1),
       While (LT (Ref "i") (LitI 5)) (Begin [
          (Bind "y" (Mul (Ref "y") (LitI 2))),
          (Bind "i" (Add (Ref "i") (LitI 1)))
       ])]

--Type error case
ex5:: Expr
ex5 = (LTE (LitI 5) (LitS "56"))



--Expression type check
typeExpr :: Expr -> Env -> Maybe Type
typeExpr (LitI _)   _  = Just TInt
typeExpr (LitS _)   _  = Just TString
typeExpr (LitB _)   _  = Just TBool
typeExpr (Ref v)  env  = (ref2 v env)
typeExpr (Add l r) env = case (typeExpr l env, typeExpr r env) of
                          (Just TInt, Just TInt) -> Just TInt
                          _                     -> Nothing
typeExpr (Sub l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (Mul l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (LT l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (LTE l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (EQ l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (GTE l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (GT l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (NE l r) env = case (typeExpr l env, typeExpr r env) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing


--Statment type check

typeStmt :: Stmt -> Env -> Bool
typeStmt (Declare v e)     env = True      --well I don't think we have to check the type of "Declare"
typeStmt (Bind v e)        env = case (ref2 v env, typeExpr e env) of
                                  (Just tv, Just te) -> tv == te
                                  _ -> False
typeStmt (IfElse c st se)  env = case typeExpr c env of
                                   Just TBool -> typeStmt st env && typeStmt se env
                                   _ -> False
typeStmt (While c sb)      env = case typeExpr c env of
                                   Just TBool -> typeStmt sb env
                                   _ -> False
typeStmt (Begin ss)        env = all (\s -> typeStmt s env) ss


--List of statment check
progType :: [Stmt] -> Env -> Bool
progType [] _ = True
progType (s:ss) env = if (typeStmt s env) == False then False else progType ss env








-- | And Or

and :: Expr -> Expr -> Env -> Env -> Value
and a b env env' = case expr a env of
           B False -> B False
           B True -> case expr b env' of
                      B True -> B True
                      B False -> B False
                      _ -> Error
           _ -> Error


or :: Expr -> Expr -> Env -> Env -> Value
or a b env env' = case expr a env of
          B True -> B True
          B False -> case expr b env' of
                     B True -> B True
                     B False -> B False
                     _ -> Error
          _ -> Error
