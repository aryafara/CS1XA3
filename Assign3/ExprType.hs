{-
Module : ExprType
Description: A class to facilitate math evaluation! (calculus in particular)
Copyright: (c) aryafara @ 2018
License : BSD
Stability : experimental (not at all)
Portability : MSDOS (will be ported to Linux)
-}
module ExprType where
{-
Simple type combinations,
any unneccessary (for a regular algebraic calculus) 
ids/functions are added so as to make my life easier
otherwise you have the essential transcendentals (I am considering pi in a future vers)
-}
import           Data.List
data Expr a = Add  (Expr a) (Expr a) --Add  - binary addition
            | Mult (Expr a) (Expr a) --Mult - binary scalar multiplication
            | Exp  (Expr a) (Expr a) --Exp  - Expression/Polynomial to some power (useful for identities)
            | N    (Expr a)          --N    - Negative of an expression (useful for identities)
            | Nat  (Expr a)          --Nat  - natural exponential fn
            | Ln   (Expr a)          --Ln   - inverse of natural exp fn
            | Cos  (Expr a)          --Cos  - adjacent/hypotenuse trig function (useful for identities)
            | Sin  (Expr a)          --Sin  - opposite/hypotenuse trig function
            | Cons a                 --Cons  - constant value 
            | Var String             --Var  - uses a string a sa variable
  deriving (Eq) 

{- getVars
Given an expression, retrieves a list of all variable identifiers
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 ++ getVars e2
getVars (Sub e1 e2)  = getVars e1 ++ getVars e2
getVars (Mult e1 e2) = getVars e1 ++ getVars e2
getVars (Exp e1 e2)  = getVars e1 ++ getVars e2
getVars (N e)        = getVars e
getVars (Nat e)      = getVars e
getVars (Ln e)       = getVars e
getVars (Cos e)      = getVars e
getVars (Sin e)      = getVars e
getVars (e1 e2)      = getVars e1 ++ getVars e2
getVars (Cons _)     = []
getVars (Var x)      = [x]
