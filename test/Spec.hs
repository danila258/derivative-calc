module Main (main) where

import Diff (diff)
import Eval (evalExpr, substitute)
import Expr (Expr(..))
import Parser (parseExpr)
import Pretty (prettyExpr)
import Repl (LineResult(..), initialState, processLine)
import Simplify (simplify)

main :: IO ()
main = do
    runTest "basic operations" testBasicOperations
    runTest "trigonometric functions" testTrigonometricFunctions
    runTest "extended trig functions" testExtendedTrigFunctions
    runTest "inverse trig functions" testInverseTrigFunctions
    runTest "complex derivatives" testComplexDerivatives
    runTest "expressions with braces" testExpressionsWithBraces
    runTest "exponentials" testExponentials
    runTest "simplification rules" testSimplificationRules
    runTest "repl sequence" testReplSequence
    runTest "repl errors" testReplErrors
    runTest "repl extended operations" testReplExtendedOperations
    runTest "repl braces negative" testReplBracesNegative
    runTest "parse errors" testParseErrors

runTest :: String -> IO () -> IO ()
runTest name action = do
    action
    putStrLn ("PASS: " ++ name)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = pure ()
    | otherwise = error (label ++ "\nexpected: " ++ show expected ++ "\nactual:   " ++ show actual)

assertApprox :: String -> Double -> Double -> IO ()
assertApprox label expected actual
    | abs (expected - actual) <= 1.0e-9 = pure ()
    | otherwise = error (label ++ "\nexpected: " ++ show expected ++ "\nactual:   " ++ show actual)

assertLeft :: Show b => String -> Either String b -> IO ()
assertLeft label value = case value of
    Left _ -> pure ()
    Right actual -> error (label ++ "\nexpected: Left\nactual:   " ++ show actual)

testCase :: String -> String -> Double -> String -> Double -> Double -> IO ()
testCase label expression xValue expectedDerivative expectedDerivativeValue expectedFunctionValue =
    case parseExpr expression of
        Left err -> error (label ++ "\nparse error: " ++ err)
        Right expr -> do
            let derivativeExpr = simplify (diff "x" expr)
            let derivativeText = prettyExpr derivativeExpr
            assertEqual (label ++ " derivative print") expectedDerivative derivativeText
            derivativeValue <- evalAt "x" xValue derivativeExpr
            assertApprox (label ++ " derivative value") expectedDerivativeValue derivativeValue
            functionValue <- evalAt "x" xValue expr
            assertApprox (label ++ " function value") expectedFunctionValue functionValue

evalAt :: String -> Double -> Expr -> IO Double
evalAt variable value expr =
    case evalExpr (substitute variable value expr) of
        Left err -> error ("evaluation error: " ++ err)
        Right result -> pure result

testBasicOperations :: IO ()
testBasicOperations = do
    testCase "add" "x + 2" 3 "1" 1 5
    testCase "subtract" "x - 2" 3 "1" 1 1
    testCase "multiply" "x * 2" 3 "2" 2 6
    testCase "divide" "x / 2" 4 "0.5" 0.5 2
    testCase "power" "x^2" 3 "2*x" 6 9

testTrigonometricFunctions :: IO ()
testTrigonometricFunctions = do
    testCase "sin" "sin(x)" 0 "cos(x)" 1 0
    testCase "cos" "cos(x)" 0 "-sin(x)" 0 1

testExtendedTrigFunctions :: IO ()
testExtendedTrigFunctions = do
    testCase "tan" "tan(x)" 0 "1/cos(x)^2" 1 0
    testCase "cot" "cot(x)" 1 "-1/sin(x)^2" (-1 / (sin 1 ** 2)) (cos 1 / sin 1)

testInverseTrigFunctions :: IO ()
testInverseTrigFunctions = do
    testCase "arcsin" "arcsin(x)" 0 "(1 - x^2)^-0.5" 1 0
    testCase "arccos" "arccos(x)" 0 "-(1 - x^2)^-0.5" (-1) (pi / 2)
    testCase "arctan" "arctan(x)" 1 "1/(1 + x^2)" 0.5 (atan 1)
    testCase "arccot" "arccot(x)" 1 "-1/(1 + x^2)" (-0.5) (atan 1)

testComplexDerivatives :: IO ()
testComplexDerivatives = do
    testCase "product" "x * x" 4 "x + x" 8 16
    testCase "quotient" "x / x" 5 "(x - x)/x^2" 0 1

testExpressionsWithBraces :: IO ()
testExpressionsWithBraces = do
    testCase "braces" "x^2 + (1/x)" 10 "2*x - 1/x^2" 19.99 100.1

testExponentials :: IO ()
testExponentials = do
    testCase "ln" "ln(x)" 2 "1/x" 0.5 (log 2)
    testCase "exp" "exp(x)" 1 "exp(x)" (exp 1) (exp 1)
    testCase "e^x" "e^x" 2 "e^x" (exp 2) (exp 2)
    testCase "a^x" "2^x" 3 "2^x*log(2)" (8 * log 2) 8

testSimplificationRules :: IO ()
testSimplificationRules = do
    assertEqual
        "simplify neg division"
        "-1/sin(x)^2"
        (prettyExpr (simplify (Neg (Div (Const 1) (Pow (Sin (Var "x")) (Const 2))))))
    assertEqual
        "simplify neg fraction"
        "-1/(1 + x^2)"
        (prettyExpr (simplify (Neg (Div (Const 1) (Add (Const 1) (Pow (Var "x") (Const 2)))))))
    assertEqual
        "simplify neg product"
        "-(x*y)"
        (prettyExpr (simplify (Neg (Mul (Var "x") (Var "y")))))

testReplSequence :: IO ()
testReplSequence = do
    let inputs =
            [ "F(x) = x^2 + (1/x)"
            , "F(10)"
            , "F'(10)"
            , "F"
            , "F'"
            , "exit"
            ]
    let expectedOutputs =
            [ "100.1"
            , "19.99"
            , "x^2 + 1/x"
            , "2*x - 1/x^2"
            ]
    let actualOutputs = runRepl inputs
    assertEqual "repl outputs" expectedOutputs actualOutputs

testReplErrors :: IO ()
testReplErrors = do
    let inputs =
            [ "F"
            , "F(1)"
            , "F'(1)"
            , "G(x) = x + 1"
            , "F"
            , "G"
            , "G(abc)"
            , "G(2)"
            , "exit"
            ]
    let expectedOutputs =
            [ "No function defined."
            , "No function defined."
            , "No function defined."
            , "No function defined."
            , "x + 1"
            , "Invalid numeric argument."
            , "3.0"
            ]
    let actualOutputs = runRepl inputs
    assertEqual "repl error outputs" expectedOutputs actualOutputs

testReplExtendedOperations :: IO ()
testReplExtendedOperations = do
    let one = 1 :: Double
    let two = 2 :: Double
    let exp2 = (exp one) ** two
    let inputs =
            [ "F(x) = tan(x)", "F", "F'", "F(0)", "F'(0)"
            , "F(x) = cot(x)", "F", "F'", "F(1)", "F'(1)"
            , "F(x) = arcsin(x)", "F", "F'", "F(0)", "F'(0)"
            , "F(x) = arccos(x)", "F", "F'", "F(0)", "F'(0)"
            , "F(x) = arctan(x)", "F", "F'", "F(1)", "F'(1)"
            , "F(x) = arccot(x)", "F", "F'", "F(1)", "F'(1)"
            , "F(x) = ln(x)", "F", "F'", "F(1)", "F'(1)"
            , "F(x) = 2^x", "F", "F'", "F(3)", "F'(3)"
            , "F(x) = e^x", "F", "F'", "F(2)", "F'(2)"
            ]
    let expectedOutputs =
            [ "tan(x)", "1/cos(x)^2", "0.0", "1.0"
            , "cot(x)", "-1/sin(x)^2", show (cos one / sin one), show (-1 / (sin one ** 2))
            , "arcsin(x)", "(1 - x^2)^-0.5", "0.0", "1.0"
            , "arccos(x)", "-(1 - x^2)^-0.5", show (pi / (2 :: Double)), "-1.0"
            , "arctan(x)", "1/(1 + x^2)", show (atan one), "0.5"
            , "arccot(x)", "-1/(1 + x^2)", show (atan one), "-0.5"
            , "log(x)", "1/x", "0.0", "1.0"
            , "2^x", "2^x*log(2)", "8.0", show (8 * log two)
            , "e^x", "e^x", show exp2, show exp2
            ]
    let actualOutputs = runRepl inputs
    assertEqual "repl extended outputs" expectedOutputs actualOutputs

testReplBracesNegative :: IO ()
testReplBracesNegative = do
    let inputs =
            [ "F(x) = (x + 1) + (-2)"
            , "F"
            , "F'"
            , "F(3)"
            , "F'(3)"
            , "exit"
            ]
    let expectedOutputs =
            [ "x + 1 - 2"
            , "1"
            , "2.0"
            , "1.0"
            ]
    let actualOutputs = runRepl inputs
    assertEqual "repl braces negative outputs" expectedOutputs actualOutputs

testParseErrors :: IO ()
testParseErrors = do
    assertLeft "missing close paren" (parseExpr "(x + 1")
    assertLeft "invalid number" (parseExpr "1..2")
    assertLeft "unknown function" (parseExpr "foo(x)")

runRepl :: [String] -> [String]
runRepl = go initialState []
  where
    go _ acc [] = reverse acc
    go state acc (line:rest) =
        let result = processLine state line
            acc' = reverse (outputs result) ++ acc
         in if continue result
            then go (nextState result) acc' rest
            else reverse acc'
