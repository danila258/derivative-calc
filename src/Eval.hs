module Eval
    ( evalExpr
    , substitute
    ) where

import Expr (Expr(..))

-- Считаем уже готовое дерево в число, если в нём нет свободных переменных
evalExpr :: Expr -> Either String Double
evalExpr expr = case expr of
    Const value -> Right value
    ConstE -> Right (exp 1)
    Var name -> Left ("unbound variable: " ++ name)
    Add a b -> (+) <$> evalExpr a <*> evalExpr b
    Sub a b -> (-) <$> evalExpr a <*> evalExpr b
    Mul a b -> (*) <$> evalExpr a <*> evalExpr b
    Div a b -> (/) <$> evalExpr a <*> evalExpr b
    Pow a b -> (**) <$> evalExpr a <*> evalExpr b
    Sin a -> sin <$> evalExpr a
    Cos a -> cos <$> evalExpr a
    Tan a -> tan <$> evalExpr a
    Cot a -> cot <$> evalExpr a
    ASin a -> asin <$> evalExpr a
    ACos a -> acos <$> evalExpr a
    ATan a -> atan <$> evalExpr a
    ACot a -> acot <$> evalExpr a
    Exp a -> exp <$> evalExpr a
    Log a -> log <$> evalExpr a
    Neg a -> negate <$> evalExpr a

cot :: Double -> Double
cot value = cos value / sin value

acot :: Double -> Double
acot value = atan (1 / value)

-- Подстановка x = value перед вычислением, чтобы REPL не делал это вручную в каждом месте
substitute :: String -> Double -> Expr -> Expr
substitute variable value expr = case expr of
    Const _ -> expr
    ConstE -> expr
    Var name -> if name == variable then Const value else expr
    Add a b -> Add (substitute variable value a) (substitute variable value b)
    Sub a b -> Sub (substitute variable value a) (substitute variable value b)
    Mul a b -> Mul (substitute variable value a) (substitute variable value b)
    Div a b -> Div (substitute variable value a) (substitute variable value b)
    Pow a b -> Pow (substitute variable value a) (substitute variable value b)
    Sin a -> Sin (substitute variable value a)
    Cos a -> Cos (substitute variable value a)
    Tan a -> Tan (substitute variable value a)
    Cot a -> Cot (substitute variable value a)
    ASin a -> ASin (substitute variable value a)
    ACos a -> ACos (substitute variable value a)
    ATan a -> ATan (substitute variable value a)
    ACot a -> ACot (substitute variable value a)
    Exp a -> Exp (substitute variable value a)
    Log a -> Log (substitute variable value a)
    Neg a -> Neg (substitute variable value a)
