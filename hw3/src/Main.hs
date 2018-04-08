module Main where

import SyntaxAnalyzer

main :: IO ()
main = do
    putStrLn "Enter expression:"
    expr <- getLine
    putStrLn $ show $ analyzeExpression expr