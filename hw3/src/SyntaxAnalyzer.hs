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
import Control.Applicative

data Expr
    = Plus Expr Expr
    | Minus Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Number Int
    deriving (Show, Eq)

-- Main analyzer interface.
-- Transforms given string to AST.
-- Report an error if given string is not a correct arithmetic expression.
analyzeExpression :: String -> Expr
analyzeExpression str = buildExprAST $ runLexer str

buildExprAST :: [Lexeme] -> Expr
buildExprAST tokens = case parse parseExpr tokens of
    Right ([], result)   -> result
    Right (rest, result) -> error ("Failed failed to parse till the end of the line." ++ (show rest))
    Left  err            -> error err

-- Parser for expression.
parseExpr :: Parser Lexeme Expr
parseExpr = parsePlusMinus <|> parseMulDiv <|> parsePow <|> parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse plus, minus priority expression.
-----------------------------------------------------------------------

parsePlusMinus :: Parser Lexeme Expr
parsePlusMinus = Parser f where
    f tokens = case parse parsePlusMinusOperand tokens of
        Right (ts, result) -> parsePlusMinusHelper result ts
        other              -> other

parsePlusMinusHelper :: Expr -> [Lexeme] -> Either String ([Lexeme], Expr)
parsePlusMinusHelper left tokens
    | tokens == []               = Right ([], left)
    | head tokens == Lexer.Minus =
        case parse parsePlusMinusOperand (tail tokens) of
            Right (ts, result) -> parsePlusMinusHelper (SyntaxAnalyzer.Minus left result) ts
            err                -> err
    | head tokens == Lexer.Plus  =
        case parse parsePlusMinusOperand (tail tokens) of
            Right (ts, result) -> parsePlusMinusHelper (SyntaxAnalyzer.Plus left result) ts
            err                -> err
    | otherwise                  = Right (tokens, left)

parsePlusMinusOperand :: Parser Lexeme Expr
parsePlusMinusOperand = parseMulDiv <|> parsePow <|> parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse multiply, divide priority expression.
-----------------------------------------------------------------------

parseMulDiv :: Parser Lexeme Expr
parseMulDiv = Parser f where
    f tokens = case parse parseMulDivOperand tokens of
        Right (ts, result) -> parseMulDivHelper result ts
        other              -> other

parseMulDivHelper :: Expr -> [Lexeme] -> Either String ([Lexeme], Expr)
parseMulDivHelper left tokens
    | tokens == []             = Right ([], left)
    | head tokens == Lexer.Mul =
        case parse parseMulDivOperand (tail tokens) of
            Right (ts, result) -> parseMulDivHelper (SyntaxAnalyzer.Mul left result) ts
            err                -> err
    | head tokens == Lexer.Div =
        case parse parseMulDivOperand (tail tokens) of
            Right (ts, result) -> parseMulDivHelper (SyntaxAnalyzer.Div left result) ts
            err                -> err
    | otherwise                = Right (tokens, left)

parseMulDivOperand :: Parser Lexeme Expr
parseMulDivOperand = parsePow <|> parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse power priority expression.
-----------------------------------------------------------------------

parsePow :: Parser Lexeme Expr
parsePow = (fmap (\a op b -> SyntaxAnalyzer.Pow a b) parsePowOperand) <*> parsePowOperator <*> (parsePow <|> parsePowOperand)

parsePowOperator :: Parser Lexeme Lexeme
parsePowOperator = satisfy (\op -> op == Lexer.Pow) "Failed to parse power."

parsePowOperand :: Parser Lexeme Expr
parsePowOperand = parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse expression in braces.
-----------------------------------------------------------------------

parseBraces :: Parser Lexeme Expr
parseBraces = (fmap (\_ expr _ -> expr) parseOpenBrace) <*> parseExpr <*> parseCloseBrace

parseOpenBrace :: Parser Lexeme Lexeme
parseOpenBrace = satisfy (\brace -> brace == OpenBrace) "Failed to parse open brace."

parseCloseBrace :: Parser Lexeme Lexeme
parseCloseBrace = satisfy (\brace -> brace == CloseBrace) "Failed to parse close brace."

-----------------------------------------------------------------------
-- Parse single number expression.
-----------------------------------------------------------------------

parseSingleNumber :: Parser Lexeme Expr
parseSingleNumber = fmap (\(Lexer.Number n) -> SyntaxAnalyzer.Number n) $ satisfy isNumberLexeme "Failed to parse number."
