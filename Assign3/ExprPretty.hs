{-
Module : ExprPretty
Description: A class that containing the parsers for various basic 'Expr' types
Copyright: (c) aryafara @ 2018
License : BSD
Stability : experimental (not at all)
Portability : MSDOS (will be ported to Linux)
-}
module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Cons x)     = parens $ "val " ++ show x
