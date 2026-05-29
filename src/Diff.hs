module Diff
    ( diff
    ) where

import Expr (Expr(..))

-- Рекурсивное взятие производной для всех поддерживаемых выражений
diff :: String -> Expr -> Expr
diff variable expr = case expr of
    Const _ -> Const 0
    ConstE -> Const 0
    Var name -> if name == variable then Const 1 else Const 0
    Add a b -> Add (diff variable a) (diff variable b)
    Sub a b -> Sub (diff variable a) (diff variable b)
    Mul a b -> Add (Mul (diff variable a) b) (Mul a (diff variable b))
    Div a b -> Div (Sub (Mul (diff variable a) b) (Mul a (diff variable b))) (Pow b (Const 2))
    Pow base exponentExpr -> powerRule variable base exponentExpr
    Sin a -> Mul (Cos a) (diff variable a)
    Cos a -> Mul (Neg (Sin a)) (diff variable a)
    Tan a -> Mul (Div (Const 1) (Pow (Cos a) (Const 2))) (diff variable a)
    Cot a -> Mul (Neg (Div (Const 1) (Pow (Sin a) (Const 2)))) (diff variable a)
    ASin a -> Mul (Pow (Sub (Const 1) (Pow a (Const 2))) (Const (-0.5))) (diff variable a)
    ACos a -> Mul (Neg (Pow (Sub (Const 1) (Pow a (Const 2))) (Const (-0.5)))) (diff variable a)
    ATan a -> Mul (Div (Const 1) (Add (Const 1) (Pow a (Const 2)))) (diff variable a)
    ACot a -> Mul (Neg (Div (Const 1) (Add (Const 1) (Pow a (Const 2))))) (diff variable a)
    Exp a -> Mul (Exp a) (diff variable a)
    Log a -> Div (diff variable a) a
    Neg a -> Neg (diff variable a)

-- Правило степени отдельно
powerRule :: String -> Expr -> Expr -> Expr
powerRule variable base exponentExpr =
    case exponentExpr of
        Const n -> Mul (Mul (Const n) (Pow base (Const (n - 1)))) (diff variable base)
        _ -> Mul (Pow base exponentExpr) (Add (Mul (diff variable exponentExpr) (Log base)) (Div (Mul exponentExpr (diff variable base)) base))
