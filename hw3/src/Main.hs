module Main where

import Lexer

main :: IO ()
main = do
    putStrLn "Enter expression."
    expr <- getLine
    putStrLn $ show $ runLexer expr