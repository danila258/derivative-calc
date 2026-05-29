module Expr
    ( Expr(..)
    ) where

-- Это базовое дерево выражений
data Expr
    = Const Double
    | ConstE
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Sin Expr
    | Cos Expr
    | Tan Expr
    | Cot Expr
    | ASin Expr
    | ACos Expr
    | ATan Expr
    | ACot Expr
    | Exp Expr
    | Log Expr
    | Neg Expr
    deriving (Eq, Show)
