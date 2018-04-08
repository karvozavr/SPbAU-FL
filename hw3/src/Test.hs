module Test where

import SyntaxAnalyzer
import Test.HUnit

-- Здесь могли быть ваши тесты

input  = "(0 + 13) * (( - 7) / 0)"
answer = Mul (Plus (Number 0) (Number 13)) (Div (Minus (Number 42) (Number 7)) (Number 0))

testAnalyzer :: Assertion
testAnalyzer = do
    (analyzeExpression input) @?= answer