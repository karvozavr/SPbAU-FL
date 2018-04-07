-----------------------------------------------------------------------
-- Syntax analyzer for arithmetic expressions.
--
-- Implementation: recursive descend using applicative parsers.
--
-- Usage: analyzeExpression on String - arithmetic expression.
-- It returns AST for this expression or reports parsing error.
-----------------------------------------------------------------------

module SyntaxAnalyzer where

import Parser
import Lexer

data Term = Number Int
data Expr
    = Plus Expr Expr
    | Minus Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Deg Expr Expr
    | Braces Expr
    | Term

-- Main analyzer interface.
-- Transforms given string to AST.
-- Report an error if given string is not a correct arithmetic expression.
analyzeExpression :: String -> Expr
analyzeExpression str = case parse parseExpr (runLexer str) of
    Right ([], result)  -> result
    Right (rest, _)     -> error "TODO ERROR"
    Left  err           -> error err

-- Parser for expression.
parseExpr :: Parser Lexeme Expr
parseExpr = undefined
