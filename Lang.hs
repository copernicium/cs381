module Lang where -- TODO name language

import Prelude hiding (LT, EQ, GT)

-- =========================================
-- 
-- Language Core
-- 

-- Abstract Syntax

type Var = String
type Name = String

-- | Expression type
--
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
   | CallFunc Name [Expr]
  deriving (Eq,Show)

-- | Statement type
--
data Stmt
   = Declare Var Expr
   | Bind Var Expr
   | IfElse Expr Stmt Stmt
   | While Expr Stmt -- TODO take [Stmt] instead?
   | Begin [Stmt]
   | DefineFunc Name [Var] Stmt
   | Return Expr
  deriving (Eq,Show)

-- =========================================
--
-- Examples
--

-- Good Examples

-- int x = 0
-- x = 5
ex1 :: [Stmt]
ex1 = [Declare "x" (LitI 0),
       Bind "x" (LitI 5)]

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

-- def double(x)
-- begin
--    return 2 * x
-- end
--
-- int x = double(2)
--
exFunc :: [Stmt]
exFunc = [DefineFunc "double" ["x"] (Return (Mul (LitI 2) (Ref "x"))),
          Declare "x" (CallFunc "double" [(LitI 2)])]

-- =========================================
--
-- Valuation function
--

-- | Semantic domain
--
data Value 
   = I Int 
   | S String
   | B Bool
   | Function [Var] Stmt
   | Error
  deriving (Eq,Show)

-- | Type for making named values accessible
--
type Env = [(Var, Value)] -- TODO scopes, lazy-evaluation?

-- | Resulting type of evaluating a statement
--
data EvalResult = State Env    -- All other statements affect the Env
                | Result Value -- Return statements produce Result
                | EvalError
  deriving (Eq,Show)


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


-- | Build a new Env from a list of variable names, a list of expressions (values), and the current environment (parent scope)
-- 
buildEnv :: [Var] -> [Expr] -> Env -> Maybe Env
buildEnv (name:names) (e:es) env = case buildEnv names es env of
                                     Just newEnv -> Just ((name, expr e env) : newEnv) -- Add name and evaluated expression to list
                                     Nothing -> Nothing
buildEnv [] [] _ = Just [] -- Good end -- reached end of both lists at same time
buildEnv _ _ _ = Nothing   -- Bad end  -- one list ended before the other

-- | Apply a function to a list of argument expressions within a scope
--
applyFunc :: Value -> [Expr] -> Env -> Value
applyFunc (Function params s) args env = case buildEnv params args env of         -- Build the scope for the function
                                          Just funcEnv -> case stmt s funcEnv of  -- Execute the function
                                                             Result val -> val    -- If the function does not return, then there's an error -- TODO void functions?
                                                             _ -> Error
                                          Nothing -> Error
applyFunc _ _ _ = Error -- Since we're applying a Value to a list of expressions, if that value is not of type Function, then it's a type error

-- Evaluation function for an expression
--
expr :: Expr -> Env -> Value
expr (LitI x) env               = I x
expr (LitS x) env               = S x
expr (LitB x) env               = B x
expr (Ref var) env              = ref var env
expr (Add a b) env              = case (expr a env, expr b env) of
                                    (I i, I j) -> I (i + j)
                                    (S i, S j) -> S (i ++ j)
                                    _ -> Error
expr (Sub a b) env              = arithmeticOp a b env (-)
expr (Mul a b) env              = arithmeticOp a b env (*)
expr (LT a b) env               = relationalOp a b env (<)
expr (LTE a b) env              = relationalOp a b env (<=)
expr (EQ a b) env               = relationalOp a b env (==)
expr (GTE a b) env              = relationalOp a b env (>=)
expr (GT a b) env               = relationalOp a b env (>)
expr (NE a b) env               = relationalOp a b env (/=)
expr (Ternary c t e) env        = case (expr c env) of 
                                    (B True)  -> expr t env
                                    (B False) -> expr e env
                                    _ -> Error
expr (CallFunc name params) env = applyFunc (ref name env) params env -- Call the function by name to a list of params, updating the environment

-- | Bind an existing variable to a new value
--
bind :: Var -> Value -> Env -> Maybe Env
bind var val ((name, val') : t) 
  | name == var = Just ((var, val) : t)
  | otherwise   = case bind var val t of 
                    Nothing -> Nothing
                    Just env -> Just ((name, val') : env)
bind _ _ []              = Nothing

-- | Evaluation function for a single statement
--
stmt :: Stmt -> Env -> EvalResult
stmt (Declare var e) env               = if find var env then EvalError else State ((var, expr e env) : env) -- Add variable to Env
stmt (Bind var e) env                  = case bind var (expr e env) env of
                                           Just env' -> State env'
                                           Nothing -> EvalError
stmt (IfElse c t e) env                = case expr c env of
                                           B True  -> stmt t env
                                           B False -> stmt e env
                                           _ -> EvalError
stmt (While c b) env                   = case expr c env of
                                           B True  -> case stmt b env of
                                                        State env' -> stmt (While c b) env'
                                                        Result val -> Result val -- If there was a return within the while loop, exit the loop
                                                        EvalError  -> EvalError
                                           B False -> State env
                                           _ -> EvalError
stmt (Begin b) env                     = eval b env
stmt (Return e) env                    = Result (expr e env)  -- Return statements produce a Value (Result), and don't update the environment (State)
stmt (DefineFunc name params body) env = if find name env then EvalError else State ((name, Function params body) : env) -- Add Function to Env

-- | Evaluation function for a list of statements
--
eval :: [Stmt] -> Env -> EvalResult
eval [] env = State env
eval (h:t) env = case stmt h env of
                   State env' -> eval t env'
                   Result val -> Result val
                   EvalError  -> EvalError

-- | Run a program in a new environment and produce its result
--
run :: [Stmt] -> EvalResult
run prog = eval prog [] -- TODO type-checking

-- =========================================
-- 
-- Static type system
--

data Type = TInt  -- TODO add TVoid?
          | TString 
          | TBool 
          | TFunction
          | TError
     deriving (Eq,Show)

--Helper function
ref2 :: Var -> Env -> Maybe Type
ref2 _ []      = Nothing
ref2 var ((name, val) : t) | var ==name = case val of 
                                           (I v) -> Just TInt
                                           (S s) -> Just TString
                                           (B b) -> Just TBool
                                           (Function _ _) -> Just TFunction
                                           Error -> Just TError
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
typeExpr (LitI _)   _               = Just TInt
typeExpr (LitS _)   _               = Just TString
typeExpr (LitB _)   _               = Just TBool
typeExpr (Ref v)  env               = (ref2 v env)
typeExpr (Add l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TInt
                                        _                     -> Nothing
typeExpr (Sub l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TInt
                                        _                      -> Nothing
typeExpr (Mul l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TInt
                                        _                      -> Nothing
typeExpr (LT l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (LTE l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (EQ l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (GTE l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (GT l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (NE l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (Just TInt, Just TInt) -> Just TBool
                                        _                      -> Nothing
typeExpr (Ternary c t e) env        = case typeExpr c env of
                                        Just TBool -> case (typeExpr t env, typeExpr e env) of
                                                        (tt, te) -> if tt == te then tt else Nothing
                                        _          -> Nothing
typeExpr (CallFunc name params) env = Nothing -- TODO Ensure all return statements have the same value


--Statment type check

typeStmt :: Stmt -> Env -> Bool -- TODO produce either a Type instead of Bool
typeStmt (Declare v e)               env = True      -- TODO Well I don't think we have to check the type of "Declare"
typeStmt (Bind v e)                  env = case (ref2 v env, typeExpr e env) of
                                            (Just tv, Just te) -> tv == te
                                            _ -> False
typeStmt (IfElse c st se)            env = case typeExpr c env of
                                             Just TBool -> typeStmt st env && typeStmt se env
                                             _ -> False
typeStmt (While c sb)                env = case typeExpr c env of
                                             Just TBool -> typeStmt sb env
                                             _ -> False
typeStmt (Begin ss)                  env = all (\s -> typeStmt s env) ss
typeStmt (DefineFunc name vars body) env = True -- TODO Same as "Declare", do we need to check the type?
typeStmt (Return e) env                  = case typeExpr e env of 
                                             Just _ -> True
                                             _      -> False


--List of statment check
progType :: [Stmt] -> Env -> Bool
progType [] _ = True
progType (s:ss) env = if (typeStmt s env) == False then False else progType ss env



-- =========================================
--
-- Syntactic sugar
--

true :: Expr
true = EQ (LitI 0) (LitI 0)

false :: Expr
false = EQ (LitI 0) (LitI 1)

and :: Expr -> Expr -> Expr
and l r = Ternary l r false

or :: Expr -> Expr -> Expr
or l r = Ternary l true r

neg :: Expr -> Expr
neg a = Mul (LitI (-1)) a

not :: Expr -> Expr
not a = Ternary a false true

