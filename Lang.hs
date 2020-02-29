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
ex4 :: [Stmt]
ex4 = [DefineFunc "double" ["x"] (Return (Mul (LitI 2) (Ref "x"))),
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

-- =========================================
-- 
-- Static type system
--

data Type = TInt  -- TODO add TVoid?
          | TString 
          | TBool 
          | TFunction
          | TError String
     deriving (Eq,Show)

--Helper function
typeOfRef :: Var -> Env -> Type
typeOfRef name []               = TError ("Undefined reference to name " ++ name)
typeOfRef var ((name, val) : t) | var == name = case val of 
                                            (I v) -> TInt
                                            (S s) -> TString
                                            (B b) -> TBool
                                            (Function _ _) -> TFunction
                                            Error -> TError "" -- TODO this will eventually not be needed
                                | otherwise = typeOfRef var t
                           
--Type good case
exType1:: Expr
exType1 = (Add (LitI 5) (LitI 3))


exType2 :: [Stmt]
exType2 = [Declare "i" (LitI 0),
           Declare "y" (LitI 1),
           While (LT (Ref "i") (LitI 5)) (Begin [
              (Bind "y" (Mul (Ref "y") (LitI 2))),
              (Bind "i" (Add (Ref "i") (LitI 1)))
           ])]

--Type error case

exType3:: Expr
exType3 = (Add (LitI 5) (Ref "x"))

exType4:: Expr
exType4 = (LTE (LitI 5) (LitS "56"))



--Expression type check
typeExpr :: Expr -> Env -> Type
typeExpr (LitI _)   _               = TInt
typeExpr (LitS _)   _               = TString
typeExpr (LitB _)   _               = TBool
typeExpr (Ref v)  env               = typeOfRef v env
typeExpr (Add l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TInt
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "Add -- Mismatched types"
typeExpr (Sub l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TInt
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "Sub -- Mismatched types"
typeExpr (Mul l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TInt
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "Mul -- Mismatched types"
typeExpr (LT l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "LT -- Mismatched types"
typeExpr (LTE l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "LTE -- Mismatched types"
typeExpr (EQ l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "EQ -- Mismatched types"
typeExpr (GTE l r) env              = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "GTE -- Mistmached types"
typeExpr (GT l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "GT -- Mismatched types"
typeExpr (NE l r) env               = case (typeExpr l env, typeExpr r env) of
                                        (TInt, TInt)    -> TBool
                                        (TError msg, _) -> TError msg
                                        (_, TError msg) -> TError msg
                                        _               -> TError "NE -- Mismatched types"
typeExpr (Ternary c t e) env        = case typeExpr c env of
                                        TBool -> case (typeExpr t env, typeExpr e env) of
                                                   (TError msg, _) -> TError msg
                                                   (_, TError msg) -> TError msg
                                                   (tt, te)    -> if tt == te then tt else TError ""
                                        _     -> TError "Invalid ternary conditional"
typeExpr (CallFunc name params) env = TError "TODO" -- TODO Ensure all return statements have the same value


--Statment type check

typeStmt :: Stmt -> Env -> Bool -- TODO produce either a Type instead of Bool
typeStmt (Declare v e)               env = case typeExpr e env of
                                            TError msg -> False
                                            _          -> True
typeStmt (Bind v e)                  env = case (typeOfRef v env, typeExpr e env) of
                                            (TError msg, _) -> False
                                            (_, TError msg) -> False
                                            (tv, te)        -> tv == te
typeStmt (IfElse c st se)            env = case typeExpr c env of
                                             TBool -> typeStmt st env && typeStmt se env
                                             _     -> False
typeStmt (While c sb)                env = case typeExpr c env of
                                             TBool -> typeStmt sb env
                                             _     -> False
typeStmt (Begin ss)                  env = all (\s -> typeStmt s env) ss
typeStmt (DefineFunc name vars body) env = True -- TODO
typeStmt (Return e) env                  = case typeExpr e env of 
                                             TError msg -> False
                                             _          -> True


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

-- =========================================
-- 
-- Prelude
--

-- | Prelude--standard library for the language
--
prelude :: [Stmt]
prelude = [
  DefineFunc "min" ["i", "j"] (Return (Ternary (LT (Ref "i") (Ref "j")) (Ref "i") (Ref "j"))),
  DefineFunc "max" ["i", "j"] (Return (Ternary (LT (Ref "i") (Ref "j")) (Ref "j") (Ref "i"))),
  DefineFunc "pow" ["base", "exp"] (Begin [
    IfElse (LTE (Ref "expr") (LitI 0))
      (Return (LitI 1))
      (Return (Mul (Ref "base") (CallFunc "pow" [(Ref "base"), Sub (Ref "exp") (LitI 1)])))
  ])
  ]

exPrelude1 :: [Stmt]
exPrelude1 = [Declare "x" (CallFunc "min" [LitI 7, LitI 6])]

exPrelude2 :: [Stmt]
exPrelude2 = [Declare "x" (CallFunc "pow" [LitI 2, LitI 5])]

-- =========================================
-- 
-- Top-level interpretter
--



-- | Run a program in a new environment and produce its result
--
run :: [Stmt] -> EvalResult
run prog = if progType (prelude ++ prog) [] then eval (prelude ++ prog) [] else EvalError-- TODO type-checking
