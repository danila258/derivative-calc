module Pretty
    ( prettyExpr
    ) where

import Expr (Expr(..))

-- Печатаем AST обратно в читаемом виде с учётом приоритетов
prettyExpr :: Expr -> String
prettyExpr = go 0

go :: Int -> Expr -> String
go _ (Const value) = prettyNumber value
go _ ConstE = "e"
go _ (Var name) = name
go precedence (Neg a) = wrap (precedence > 3) ('-':go 3 a)
go precedence (Add a b) = wrap (precedence > 1) (go 1 a ++ addSeparator b)
go precedence (Sub a b) = wrap (precedence > 1) (go 1 a ++ " - " ++ go 2 b)
go precedence (Mul a b) = wrap (precedence > 2) (go 2 a ++ "*" ++ go 2 b)
go precedence (Div a b) = wrap (precedence > 2) (go 2 a ++ "/" ++ go 3 b)
go precedence (Pow a b) = wrap (precedence > 3) (go 3 a ++ "^" ++ go 4 b)
go precedence (Sin a) = wrap (precedence > 3) ("sin(" ++ go 0 a ++ ")")
go precedence (Cos a) = wrap (precedence > 3) ("cos(" ++ go 0 a ++ ")")
go precedence (Tan a) = wrap (precedence > 3) ("tan(" ++ go 0 a ++ ")")
go precedence (Cot a) = wrap (precedence > 3) ("cot(" ++ go 0 a ++ ")")
go precedence (ASin a) = wrap (precedence > 3) ("arcsin(" ++ go 0 a ++ ")")
go precedence (ACos a) = wrap (precedence > 3) ("arccos(" ++ go 0 a ++ ")")
go precedence (ATan a) = wrap (precedence > 3) ("arctan(" ++ go 0 a ++ ")")
go precedence (ACot a) = wrap (precedence > 3) ("arccot(" ++ go 0 a ++ ")")
go precedence (Exp a) = wrap (precedence > 3) ("exp(" ++ go 0 a ++ ")")
go precedence (Log a) = wrap (precedence > 3) ("log(" ++ go 0 a ++ ")")

wrap :: Bool -> String -> String
wrap True text = "(" ++ text ++ ")"
wrap False text = text

prettyNumber :: Double -> String
prettyNumber value
    | fromInteger (round value) == value = show (round value :: Integer)
    | otherwise = show value

-- Если следующий кусок отрицательный, лучше показать его как вычитание
addSeparator :: Expr -> String
addSeparator (Neg inner) = " - " ++ go 2 inner
addSeparator (Const value)
    | value < 0 = " - " ++ prettyNumber (negate value)
    | otherwise = " + " ++ prettyNumber value
addSeparator other = " + " ++ go 1 other
