module Simplify
    ( simplify
    ) where

import Expr (Expr(..))

simplify :: Expr -> Expr
simplify expr = case expr of
    Const _ -> expr
    ConstE -> expr
    Var _ -> expr
    Neg a -> case simplify a of
        Const value -> Const (-value)
        Neg inner -> simplify inner
        Div num den -> simplifyDiv (Neg num) den
        Mul left right -> simplifyMul (Neg left) right
        other -> Neg other
    Add a b -> simplifyAdd (simplify a) (simplify b)
    Sub a b -> simplifySub (simplify a) (simplify b)
    Mul a b -> simplifyMul (simplify a) (simplify b)
    Div a b -> simplifyDiv (simplify a) (simplify b)
    Pow a b -> simplifyPow (simplify a) (simplify b)
    Sin a -> Sin (simplify a)
    Cos a -> Cos (simplify a)
    Tan a -> Tan (simplify a)
    Cot a -> Cot (simplify a)
    ASin a -> ASin (simplify a)
    ACos a -> ACos (simplify a)
    ATan a -> ATan (simplify a)
    ACot a -> ACot (simplify a)
    Exp a -> Exp (simplify a)
    Log a -> simplifyLog (simplify a)

simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Const 0) b = b
simplifyAdd a (Const 0) = a
simplifyAdd a (Neg b) = Sub a b
simplifyAdd (Neg a) b = Sub b a
simplifyAdd (Const a) (Const b) = Const (a + b)
simplifyAdd a b = Add a b

simplifySub :: Expr -> Expr -> Expr
simplifySub a (Const 0) = a
simplifySub a (Neg b) = Add a b
simplifySub (Const a) (Const b) = Const (a - b)
simplifySub a b = Sub a b

simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Const 0) _ = Const 0
simplifyMul _ (Const 0) = Const 0
simplifyMul (Const 1) b = b
simplifyMul a (Const 1) = a
simplifyMul (Const (-1)) b = Neg b
simplifyMul a (Const (-1)) = Neg a
simplifyMul (Neg a) b = Neg (simplifyMul a b)
simplifyMul a (Neg b) = Neg (simplifyMul a b)
simplifyMul (Const a) (Const b) = Const (a * b)
simplifyMul a b = Mul a b

simplifyDiv :: Expr -> Expr -> Expr
simplifyDiv (Const 0) _ = Const 0
simplifyDiv a (Const 1) = a
simplifyDiv (Const (-1)) b = Neg (Div (Const 1) b)
simplifyDiv a (Const (-1)) = Neg (Div a (Const 1))
simplifyDiv (Const a) (Const b) = Const (a / b)
simplifyDiv a b = Div a b

simplifyPow :: Expr -> Expr -> Expr
simplifyPow _ (Const 0) = Const 1
simplifyPow a (Const 1) = a
simplifyPow (Const a) (Const b) = Const (a ** b)
simplifyPow a b = Pow a b

simplifyLog :: Expr -> Expr
simplifyLog ConstE = Const 1
simplifyLog other = Log other
