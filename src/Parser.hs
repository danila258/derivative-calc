module Parser
    ( parseExpr
    ) where

import Data.Char (isAlphaNum, isDigit, isLetter, isSpace)
import Data.List (stripPrefix)
import Expr (Expr(..))

parseExpr :: String -> Either String Expr
parseExpr input =
    case parseAddSub (trim input) of
        Right (expr, rest)
            | all isSpace rest -> Right expr
            | otherwise -> Left ("unexpected trailing input: " ++ rest)
        Left err -> Left err

parseAddSub :: String -> Either String (Expr, String)
parseAddSub input = do
    (first, rest) <- parseMulDiv input
    go first (trimLeading rest)
  where
    go acc text =
        case trimLeading text of
            ('+':remaining) -> do
                (rhs, rest) <- parseMulDiv remaining
                go (Add acc rhs) rest
            ('-':remaining) -> do
                (rhs, rest) <- parseMulDiv remaining
                go (Sub acc rhs) rest
            other -> Right (acc, other)

parseMulDiv :: String -> Either String (Expr, String)
parseMulDiv input = do
    (first, rest) <- parsePow input
    go first (trimLeading rest)
  where
    go acc text =
        case trimLeading text of
            ('*':remaining) -> do
                (rhs, rest) <- parsePow remaining
                go (Mul acc rhs) rest
            ('/':remaining) -> do
                (rhs, rest) <- parsePow remaining
                go (Div acc rhs) rest
            other -> Right (acc, other)

parsePow :: String -> Either String (Expr, String)
parsePow input = do
    (base, rest) <- parseUnary input
    case trimLeading rest of
        ('^':remaining) -> do
            (exponentExpr, rest') <- parsePow remaining
            Right (Pow base exponentExpr, rest')
        other -> Right (base, other)

parseUnary :: String -> Either String (Expr, String)
parseUnary input =
    case trimLeading input of
        ('-':remaining) -> do
            (expr, rest) <- parseUnary remaining
            Right (Neg expr, rest)
        other -> parseAtom other

parseAtom :: String -> Either String (Expr, String)
parseAtom input =
    case trimLeading input of
        ('(':remaining) -> do
            (expr, rest) <- parseAddSub remaining
            case trimLeading rest of
                (')':after) -> Right (expr, after)
                _ -> Left "missing closing parenthesis"
        text ->
            parseFunction text
                `orElse` parseNumber text
                `orElse` parseIdentifier text

parseFunction :: String -> Either String (Expr, String)
parseFunction input =
    parseNamedFunction "arcsin" ASin input
        `orElse` parseNamedFunction "arccos" ACos input
        `orElse` parseNamedFunction "arctan" ATan input
        `orElse` parseNamedFunction "arccot" ACot input
        `orElse` parseNamedFunction "sin" Sin input
        `orElse` parseNamedFunction "cos" Cos input
        `orElse` parseNamedFunction "tan" Tan input
        `orElse` parseNamedFunction "cot" Cot input
        `orElse` parseNamedFunction "exp" Exp input
        `orElse` parseNamedFunction "ln" Log input
        `orElse` parseNamedFunction "log" Log input

parseNamedFunction :: String -> (Expr -> Expr) -> String -> Either String (Expr, String)
parseNamedFunction name ctor input =
    case stripPrefix name (trimLeading input) of
        Just remaining ->
            case trimLeading remaining of
                ('(':afterOpen) -> do
                    (arg, rest) <- parseAddSub afterOpen
                    case trimLeading rest of
                        (')':afterClose) -> Right (ctor arg, afterClose)
                        _ -> Left ("missing closing parenthesis after " ++ name)
                _ -> Left ("expected ( after " ++ name)
        Nothing -> Left ("not a " ++ name ++ " function")

parseNumber :: String -> Either String (Expr, String)
parseNumber input =
    case span isNumberChar (trimLeading input) of
        (token, rest)
            | null token -> Left "expected expression"
            | not (any isDigit token) -> Left "expected number"
            | otherwise ->
                case reads token of
                    [(value, "")] -> Right (Const value, rest)
                    _ -> Left ("invalid number: " ++ token)
  where
    isNumberChar c = isDigit c || c == '.'

parseIdentifier :: String -> Either String (Expr, String)
parseIdentifier input =
    case trimLeading input of
        [] -> Left "expected identifier"
        (first:rest)
            | isLetter first ->
                let (tailChars, remaining) = span isIdentChar rest
                    name = first : tailChars
                 in Right (identifierExpr name, remaining)
            | otherwise -> Left "expected identifier"
  where
    isIdentChar c = isAlphaNum c || c == '_'

identifierExpr :: String -> Expr
identifierExpr name
    | name == "e" = ConstE
    | otherwise = Var name

orElse :: Either String a -> Either String a -> Either String a
orElse (Right value) _ = Right value
orElse (Left _) fallback = fallback

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

trimLeading :: String -> String
trimLeading = dropWhile isSpace
