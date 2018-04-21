module Main where

import System.Environment
import System.IO
import SyntaxAnalyzer

main :: IO ()
main = do
    path <- fmap head getArgs
    expr <- readFile path
    putStrLn $ show $ analyzeExpression expr

