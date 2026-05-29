module Repl
    ( AppState
    , LineResult(..)
    , initialState
    , processLine
    ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)

import Diff (diff)
import Eval (evalExpr, substitute)
import Expr (Expr(..))
import Parser (parseExpr)
import Pretty (prettyExpr)
import Simplify (simplify)

data AppState = AppState
    { storedName :: Maybe String
    , storedVar :: Maybe String
    , storedExpr :: Maybe Expr
    }

data LineResult = LineResult
    { continue :: Bool
    , nextState :: AppState
    , outputs :: [String]
    }

initialState :: AppState
initialState = AppState Nothing Nothing Nothing

-- Основная логика распознавания команд из консоли (задание функции, вычисление, взятие производной)
processLine :: AppState -> String -> LineResult
processLine state input
    | trimmed == "exit" = LineResult False state []
    | Just (name, variable, expressionText) <- parseAssignment trimmed =
        case parseExpr expressionText of
            Left err -> LineResult True state [err]
            Right expr ->
                let nextState' = AppState (Just name) (Just variable) (Just (simplify expr))
                 in LineResult True nextState' []
    | Just evalRaw <- parseEvalRaw trimmed =
        case toCommand evalRaw of
            Left err -> LineResult True state [err]
            Right command -> handleCommand state command
    | Just command <- parseCommand trimmed =
        handleCommand state command
    | otherwise = LineResult True state ["Unrecognized command."]
  where
    trimmed = trim input

data Command
    = PrintFunction String
    | PrintDerivative String
    | EvaluateFunction String Double
    | EvaluateDerivative String Double

handleCommand :: AppState -> Command -> LineResult
handleCommand state command = case command of
    PrintFunction name ->
        case activeExpr state name of
            Nothing -> noFunction state
            Just expr -> LineResult True state [prettyExpr expr]
    PrintDerivative name ->
        case activeExpr state name of
            Nothing -> noFunction state
            Just expr ->
                case storedVar state of
                    Nothing -> noFunction state
                    Just variable ->
                        LineResult True state [prettyExpr (simplify (diff variable expr))]
    EvaluateFunction name value ->
        case activeExpr state name of
            Nothing -> noFunction state
            Just expr ->
                case storedVar state of
                    Nothing -> noFunction state
                    Just variable ->
                        -- Сначала подставляем число, потом уже считаем результат
                        respondEval state (evalExpr (substitute variable value expr))
    EvaluateDerivative name value ->
        case activeExpr state name of
            Nothing -> noFunction state
            Just expr ->
                case storedVar state of
                    Nothing -> noFunction state
                    Just variable ->
                        let derivative = simplify (diff variable expr)
                         in respondEval state (evalExpr (substitute variable value derivative))

respondEval :: AppState -> Either String Double -> LineResult
respondEval state result =
    case result of
        Left err -> LineResult True state [err]
        Right value -> LineResult True state [show value]

noFunction :: AppState -> LineResult
noFunction state = LineResult True state ["No function defined."]

activeExpr :: AppState -> String -> Maybe Expr
activeExpr state name =
    case storedName state of
        Just current | current == name -> storedExpr state
        _ -> Nothing

parseAssignment :: String -> Maybe (String, String, String)
parseAssignment input = do
    (lhs, rhsWithEquals) <- breakOn '=' input
    rhs <- stripPrefix "=" rhsWithEquals
    (name, variable) <- parseHeader lhs
    pure (name, variable, trim rhs)

parseCommand :: String -> Maybe Command
parseCommand input =
    case stripSuffix "'" input of
        Just name -> Just (PrintDerivative name)
        Nothing -> Just (PrintFunction input)

parseEvalRaw :: String -> Maybe (String, String, Bool)
parseEvalRaw = splitEval

toCommand :: (String, String, Bool) -> Either String Command
toCommand (name, valueText, isDerivative) =
    case readMaybe valueText of
        Nothing -> Left "Invalid numeric argument."
        Just value ->
            Right $ if isDerivative
                then EvaluateDerivative name value
                else EvaluateFunction name value

splitEval :: String -> Maybe (String, String, Bool)
splitEval input = do
    (name, rest) <- breakOn '(' input
    valueText <- stripPrefix "(" rest
    (value, suffix) <- breakOn ')' valueText
    _ <- stripPrefix ")" suffix
    let isDerivative = case stripSuffix "'" name of
            Just base -> base /= name
            Nothing -> False
    let cleanName = maybe name id (stripSuffix "'" name)
    pure (cleanName, value, isDerivative)

parseHeader :: String -> Maybe (String, String)
parseHeader input = do
    (name, rest) <- breakOn '(' input
    variable <- stripPrefix "(" rest >>= takeUntilClose
    pure (trim name, trim variable)

takeUntilClose :: String -> Maybe String
takeUntilClose input = do
    (inside, suffix) <- breakOn ')' input
    stripPrefix ")" suffix >>= \remaining ->
        if all isSpace remaining then Just inside else Nothing

breakOn :: Eq a => a -> [a] -> Maybe ([a], [a])
breakOn needle haystack = case break (== needle) haystack of
    (_, []) -> Nothing
    (prefix, suffix) -> Just (prefix, suffix)

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix input = reverse <$> stripPrefix (reverse suffix) (reverse input)

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace
  where
        -- Обрезаем пробелы справа вручную
    dropWhileEnd p = reverse . dropWhile p . reverse


readMaybe :: Read a => String -> Maybe a
readMaybe text = case reads text of
    [(value, remainder)] | all isSpace remainder -> Just value
    _ -> Nothing