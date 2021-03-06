module Lang where

import Prelude hiding (LT, EQ, GT, and, or, not)

-- =========================================
-- 
-- Language Core
-- 

-- Abstract Syntax

type Name = String

-- | Expression type
--
data Expr
   = LitI Int
   | LitB Bool
   | LitS String
   | Ref Name
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
   = Declare Name Expr
   | Bind Name Expr
   | IfElse Expr [Stmt] [Stmt]
   | While Expr [Stmt]
   | Begin [Stmt]
   | DeclareFunc Name Type [Type] -- Annotation for type checker for function return type and parameter types
   | DefineFunc Name [Name] [Stmt] -- Define function body
   | Return Expr
  deriving (Eq,Show)

-- =========================================
--
-- Examples
--

-- Helper functions for testing

-- | Test the returned value after evalution of statements
--
testResult :: [Stmt] -> Value -> Bool
testResult s val = case run s of
                     Result val' -> val' == val
                     _           -> False

-- | Test the value of a reference after evaluation of statements
--
testState :: [Stmt] -> Name -> Value -> Bool
testState s name val = case run s of
                         State env -> ref name env == val
                         _         -> False

-- | Test that evaluation of statements results in an error
--
testError :: [Stmt] -> Bool
testError s = case run s of
                EvalError _ -> True
                _           -> False

-- Good Examples

-- |
--   var x = 0
--   x = 5
--
--
--   >>> testState ex1 "x" (I 5)
--   True
--
ex1 :: [Stmt]
ex1 = [Declare "x" (LitI 0),
       Bind "x" (LitI 5)]
-- | 
--   var x = 4
--   var y = 5
--   var z = x + y
--
--
--   >>> testState ex2 "z" (I 9)
--   True
--
ex2 :: [Stmt]
ex2 = [Declare "x" (LitI 4),
       Declare "y" (LitI 5),
       Declare "z" (Add (Ref "x") (Ref "y"))]

-- |
--   var i = 0
--   var y = 1
--   while i < 5
--   begin
--      y = y * 2
--      i = i + 1
--   end
--
--
--   >>> testState ex3 "i" (I 5)
--   True
--
--   >>> testState ex3 "y" (I 32)
--   True
--
ex3 :: [Stmt]
ex3 = [Declare "i" (LitI 0),
       Declare "y" (LitI 1),
       While (LT (Ref "i") (LitI 5)) [
          (Bind "y" (Mul (Ref "y") (LitI 2))),
          (Bind "i" (Add (Ref "i") (LitI 1)))
       ]]
-- |
--   int double(int)
-- 
--   def double(x)
--      return 2 * x
--
--   var x = double(2)
--
--
--   >>> testState ex4 "x" (I 4)
--   True
--
ex4 :: [Stmt]
ex4 = [DeclareFunc "double" TInt [TInt],
       DefineFunc  "double" ["x"] [Return (Mul (LitI 2) (Ref "x"))],
       Declare "x" (CallFunc "double" [(LitI 2)])]

-- Bad examples

-- | 
--   var y = x // x not declared
--   
--
--   >>> testError ex5
--   True
--
ex5 :: [Stmt]
ex5 = [Declare "y" (Ref "x")]

-- | 
--   var y = 1 == True // type-error
--   
--
--   >>> testError ex6
--   True
--
ex6 :: [Stmt]
ex6 = [Declare "y" (EQ (LitI 1) (LitB True))]

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
   | Function [Name] [Stmt]
   | Error String
  deriving (Eq,Show)

-- | Type for making named values accessible
--
type Env = [(Name, Value)]

-- | Resulting type of evaluating a statement
--
data EvalResult = State Env        -- All other statements affect the Env
                | Result Value     -- Return statements produce Result
                | EvalError String
  deriving (Eq,Show)


-- | Get the value of a variable
--
ref :: Name -> Env -> Value
ref var []                = Error ("Reference error to name: " ++ var)
ref var ((name, val) : t) = if name == var then val else ref var t

-- | Check if a variable is defined
--
find :: Name -> Env -> Bool
find var env = case ref var env of 
                 Error _ -> False
                 _       -> True

-- | Apply an arithmetic operator (e.g. addition) to two expressions
--
arithmeticOp :: Expr -> Expr -> Env -> (Int -> Int -> Int) -> Value
arithmeticOp a b env op = case (expr a env, expr b env) of
                            (I i, I j) -> I (op i j)
                            _          -> Error ""

-- | Evaluate two expressions of LitI with a relational/comparison operator
--
relationalOp :: Expr -> Expr -> Env -> (Int -> Int -> Bool) -> Value
relationalOp a b env cmp = case (expr a env, expr b env) of
                             (I i, I j) -> B (cmp i j)
                             _          -> Error ""


-- | Build a new Env from a list of variable names, a list of expressions (values), and the current environment (parent scope)
-- 
buildEnv :: [Name] -> [Expr] -> Env -> Maybe Env
buildEnv (name:names) (e:es) env = case buildEnv names es env of
                                     Just newEnv -> Just ((name, expr e env) : newEnv) -- Add name and evaluated expression to list
                                     Nothing     -> Nothing
buildEnv [] [] _                 = Just [] -- Good end -- reached end of both lists at same time
buildEnv _ _ _                   = Nothing   -- Bad end  -- one list ended before the other

filterFunctions :: Env -> Env
filterFunctions (h:t) = case h of
                        (_, Function _ _) -> h : filterFunctions t
                        _                 -> filterFunctions t
filterFunctions []    = []

-- | Apply a function to a list of argument expressions within a scope
--
applyFunc :: Value -> [Expr] -> Env -> Value
applyFunc (Function params s) args env = case buildEnv params args env of                                   -- Build the scope for the function
                                          Just funcEnv -> case eval s (funcEnv ++ filterFunctions env) of   -- Execute the function
                                                             Result val -> val                              -- If the function does not return, then there's an error
                                                             _ -> Error "Function evaulation error"
                                          Nothing -> Error "Invalid parameters to function"
applyFunc (Error msg) _ _              = Error msg
applyFunc _ _ _                        = Error "Value cannot be used as function" -- Since we're applying a Value to a list of expressions, if that value is not of type Function, then it's a type error

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
                                    _ -> Error ""
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
                                    _ -> Error "Invalid expression type for ternary condition"
expr (CallFunc name params) env = applyFunc (ref name env) params env -- Call the function by name to a list of params, updating the environment

-- | Bind an existing variable to a new value
--
bind :: Name -> Value -> Env -> Maybe Env
bind var val ((name, val') : t) 
  | name == var = Just ((var, val) : t)
  | otherwise   = case bind var val t of 
                    Nothing -> Nothing
                    Just env -> Just ((name, val') : env)
bind _ _ []              = Nothing

-- | Evaluation function for a single statement
--
stmt :: Stmt -> Env -> EvalResult
stmt (Declare var e) env               = if find var env then EvalError ("Redefinition of variable " ++ var) else State ((var, expr e env) : env) -- Add variable to Env
stmt (Bind var e) env                  = case bind var (expr e env) env of
                                           Just env' -> State env'
                                           Nothing -> EvalError "Undefined variable "
stmt (IfElse c t e) env                = case expr c env of
                                           B True  -> eval t env
                                           B False -> eval e env
                                           _ -> EvalError ""
stmt (While c b) env                   = case expr c env of
                                           B True  -> case eval b env of
                                                        State env'    -> stmt (While c b) env'
                                                        Result val    -> Result val -- If there was a return within the while loop, exit the loop
                                                        EvalError msg -> EvalError msg
                                           B False -> State env
                                           _ -> EvalError ""
stmt (Begin b) env                     = eval b env
stmt (Return e) env                    = Result (expr e env)  -- Return statements produce a Value (Result), and don't update the environment (State)
stmt (DeclareFunc _ _ _) env           = State env -- This statement is just used as type annotation
stmt (DefineFunc name params body) env = if find name env then EvalError ("Redefinition of function " ++ name) else State ((name, Function params body) : env) -- Add Function to Env

-- | Evaluation function for a list of statements
--
eval :: [Stmt] -> Env -> EvalResult
eval [] env    = State env
eval (h:t) env = case stmt h env of
                   State env'    -> eval t env'
                   Result val    -> Result val
                   EvalError msg -> EvalError msg

-- =========================================
-- 
-- Static type system
--

data Type = TInt
          | TString 
          | TBool 
          | TFunction Type [Type]
          | TError String
     deriving (Eq,Show)

-- | Type for making named values accessible
--
type TEnv = [(Name, Type)]

data EvalType = TResult Type
              | TVoid TEnv
              | TEvalError String
     deriving (Eq,Show)

--Helper function
typeOfRef :: Name -> TEnv -> Type
typeOfRef name []                = TError ("Undefined reference to name " ++ name)
typeOfRef var ((name, tval) : t) | var == name = tval
                                 | otherwise   = typeOfRef var t
                           
--Type good case
exType1:: Expr
exType1 = (Add (LitI 5) (LitI 3))


exType2 :: [Stmt]
exType2 = [Declare "i" (LitI 0),
           Declare "y" (LitI 1),
           While (LT (Ref "i") (LitI 5)) [
              (Bind "y" (Mul (Ref "y") (LitI 2))),
              (Bind "i" (Add (Ref "i") (LitI 1)))
           ]]

--Type error case

exType3:: Expr
exType3 = (Add (LitI 5) (Ref "x"))

exType4:: Expr
exType4 = (LTE (LitI 5) (LitS "56"))

-- |
--   int double(int)
-- 
--   def double(x,y)
--      if false
--        // Do nothing -- Bad, may not always return!
--      else
--        return 1
--
--   var x = double(0)
--
--
--   >>> testError exType5
--   True
--
exType5 :: [Stmt]
exType5 = [DeclareFunc "double" TInt [TInt],
           DefineFunc  "double" ["x"] [IfElse false [nop] [Return (LitI 1)], nop],
           Declare "x" (CallFunc "double" [(LitI 0)])]

-- | 
--   if false
--      return "BAD" // Bad, may not always return the same type
--   else
--      return 1
--
--
--   >>> testError exType6
--   True
--
exType6 :: [Stmt]
exType6 = [IfElse false [Return (LitS "BAD")] [Return (LitI 1)]]

-- | Build a type environment by matching a list of names with a list of types 
-- 
buildTEnv :: [Name] -> [Type] -> TEnv -> Maybe TEnv
buildTEnv (name:names) (t:ts) env = case buildTEnv names ts env of
                                     Just newEnv -> Just ((name, t) : newEnv) -- Add name and evaluated expression to list
                                     Nothing     -> Nothing
buildTEnv [] [] _                 = Just [] -- Good end -- reached end of both lists at same time
buildTEnv _ _ _                   = Nothing -- Bad end  -- one list ended before the other

-- | Construct a new environment from only the functions in an existing environment
--
filterTFunctions :: TEnv -> TEnv
filterTFunctions (h:t) = case h of
                        (_, TFunction _ _) -> h : filterTFunctions t
                        _                 -> filterTFunctions t
filterTFunctions []    = []

-- | Ensure that the types of each supplied expression match the supplied type list
--
--   Used in type-checking function arguments
--
matchTypes :: [Type] -> [Expr] -> TEnv -> Bool
matchTypes (t:ts) (e:es) env = if typeExpr e env == t then matchTypes ts es env else False
matchTypes [] [] _           = True  -- Good end -- reached end of both lists at same time
matchTypes _ _ _             = False -- Bad end  -- one list ended before the other

-- | Type-check an expression
--
typeExpr :: Expr -> TEnv -> Type
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
                                        TBool      -> case (typeExpr t env, typeExpr e env) of
                                                        (TError msg, _) -> TError msg
                                                        (_, TError msg) -> TError msg
                                                        (tt, te)    -> if tt == te then tt else TError ""
                                        TError msg -> TError msg
                                        _          -> TError "Invalid ternary conditional"
typeExpr (CallFunc name params) env = case typeOfRef name env of 
                                        (TFunction returnType paramTypes) -> if matchTypes paramTypes params env 
                                                                             then returnType
                                                                             else TError ("Mismatched argument types to function " ++ name)
                                        _                        -> TError ("Cannot use name " ++ name ++ " as function")


-- | Type-check a single statment
--
typeStmt :: Stmt -> TEnv -> EvalType
typeStmt (Declare v e)               env = case typeOfRef v env of  -- Ensure the name v is not in use
                                             TError _ -> case typeExpr e env of
                                                           TError msg -> TEvalError msg
                                                           t          -> TVoid ((v, t) : env)
                                             _        -> TEvalError ("Redefinition of variable " ++ v)
typeStmt (Bind v e)                  env = case (typeOfRef v env, typeExpr e env) of
                                            (TError msg, _) -> TEvalError msg
                                            (_, TError msg) -> TEvalError msg
                                            (tv, te)        -> if tv == te then TVoid env else TEvalError ("Mismatched types binding name " ++ v)
typeStmt (IfElse c st se)            env = case typeExpr c env of
                                             TBool      -> case progType st env of
                                                             TEvalError msg -> TEvalError msg
                                                             TVoid _        -> progType se env
                                                             TResult t      -> case  progType se env of
                                                                                 TResult t' -> if t' == t then TResult t else TEvalError "Inconsistent return types across if statement"
                                                                                 x -> x
                                             TError msg -> TEvalError msg
                                             _          -> TEvalError "Invalid if statement conditional"
typeStmt (While c sb)                env = case typeExpr c env of
                                             TBool      -> progType sb env
                                             TError msg -> TEvalError msg
                                             _     -> TEvalError "Invalid while statement conditional"
typeStmt (Begin ss)                  env = progType ss env
typeStmt (DeclareFunc name t pts)    env = case typeOfRef name env of -- Ensure the function name is not in use
                                             TError _ -> TVoid ((name, TFunction t pts) : env)
                                             _        -> TEvalError ("Redefintion of function " ++ name)
typeStmt (DefineFunc name vars body) env = case typeOfRef name env of -- Ensure the function has been declared
                                             TFunction t pts -> case buildTEnv vars pts env of
                                                                  Just newEnv -> case progType body (newEnv ++ filterTFunctions env) of
                                                                                 TResult t' -> if t' == t then TVoid env else TEvalError ("Function does not return declared type: " ++ name)
                                                                                 TVoid _    -> TEvalError ("Function does not return: " ++ name)
                                                                                 err        -> err
                                                                  _           -> TEvalError ("Mismatched function parameters between declaration and defintion: " ++ name)
                                             _               -> TEvalError ("Function is missing type declaration: " ++ name)
typeStmt (Return e) env                  = case typeExpr e env of
                                             TError msg -> TEvalError msg
                                             t          -> TResult t


-- | Type-check a list of statements
--
progType :: [Stmt] -> TEnv -> EvalType
progType [] env          = TVoid env
progType (s:ss) env = case typeStmt s env of
                        TEvalError msg -> TEvalError msg
                        TVoid env'     -> progType ss env'
                        TResult t      -> case ss of 
                                            [] -> TResult t
                                            _  -> case progType ss env of 
                                                    TResult t'     -> if t == t' then progType ss env else TEvalError "Inconsistent function/program return types"
                                                    TEvalError msg -> TEvalError msg
                                                    TVoid _        -> TEvalError "Not all paths in function/program return"



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

nop :: Stmt
nop = Begin []

-- =========================================
-- 
-- Prelude
--

-- | Prelude--standard library for the language
--
prelude :: [Stmt]
prelude = [
  DeclareFunc "min" TInt [TInt, TInt],
  DefineFunc "min" ["i", "j"] [Return (Ternary (LT (Ref "i") (Ref "j")) (Ref "i") (Ref "j"))],
  DeclareFunc "max" TInt [TInt, TInt],
  DefineFunc "max" ["i", "j"] [Return (Ternary (LT (Ref "i") (Ref "j")) (Ref "j") (Ref "i"))],
  DeclareFunc "pow" TInt [TInt, TInt],
  DefineFunc "pow" ["base", "exp"][
    IfElse (LTE (Ref "exp") (LitI 0))
      [Return (LitI 1)]
      [Return (Mul (Ref "base") (CallFunc "pow" [(Ref "base"), Sub (Ref "exp") (LitI 1)]))]
  ]
  ]

-- Good examples

-- |
--
--   var x = min(7, 6)
--
--   >>> testState exPrelude1 "x" (I 6)
--   True
--
exPrelude1 :: [Stmt]
exPrelude1 = [Declare "x" (CallFunc "min" [LitI 7, LitI 6])]

-- | 
--
--   var x = pow(2, 5)
--
--   >>> testState exPrelude2 "x" (I 32)
--   True
--
exPrelude2 :: [Stmt]
exPrelude2 = [Declare "x" (CallFunc "pow" [LitI 2, LitI 5])]

-- Bad examples

-- | 
--
--    var x = pow(2, "cat") // Type error
--    
--    >>> testError exPrelude3
--    True
--
exPrelude3 :: [Stmt]
exPrelude3 = [Declare "x" (CallFunc "pow" [LitI 2, LitS "cat"])]

-- | 
--    
--    x = min(2, 3) // Undeclared variable "x"
--
--    >>> testError exPrelude4
--    True
--
exPrelude4 :: [Stmt]
exPrelude4 = [Bind "x" (CallFunc "max" [LitI 2, LitI 5])]

-- =========================================
-- 
-- Top-level interpretter
--



-- | Run a program in a new environment and produce its result
--
run :: [Stmt] -> EvalResult
run prog = case progType (prelude ++ prog) [] of
             TEvalError msg -> EvalError ("Type error: " ++ msg)
             _              -> eval (prelude ++ prog) []
