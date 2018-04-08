-----------------------------------------------------------------------
-- Syntax analyzer for arithmetic expressions.
--
-- Implementation: recursive descend using applicative parsers.
--
-- Usage: analyzeExpression on String - arithmetic expression.
-- It returns AST for this expression or reports parsing error.
-----------------------------------------------------------------------

module SyntaxAnalyzer2 where

import Parser
import Lexer
import Control.Applicative

data Expr
    = Plus Expr Expr
    | Minus Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Deg Expr Expr
    | Number Int
    deriving (Show, Eq)

-- Main analyzer interface.
-- Transforms given string to AST.
-- Report an error if given string is not a correct arithmetic expression.
analyzeExpression :: String -> Expr
analyzeExpression str = case parse parseExpr (runLexer str) of
    Right ([], result)  -> result
    Right (rest, _)     -> error ("Failed to parse all input. " ++ (show rest))
    Left  err           -> error err

-- Parser for expression.
parseExpr :: Parser Lexeme Expr
parseExpr =  parsePlus <|> parseMinus <|> parseMul <|> parseDiv <|> parseSingleNumber <|> parseBraces

-- Optimizes binary operation parsing:
-- if failed to parse operation because of the end of line, return left operand as parsed expression.
optimizeBinOp :: (Expr -> Expr -> Expr) -> Parser Lexeme Expr -> Parser Lexeme (Maybe Lexeme -> Expr -> Expr)
optimizeBinOp constructor = fmap (optimizeBinOpHelper constructor)

optimizeBinOpHelper :: (Expr -> Expr -> Expr) -> Expr -> Maybe Lexeme -> Expr -> Expr
optimizeBinOpHelper constructor a operation b | operation == Nothing = a
                                              | otherwise            = constructor a b

parseBinOp :: Lexeme -> (Expr -> Expr -> Expr) -> Parser Lexeme Expr -> Parser Lexeme Expr -> Parser Lexeme Expr
parseBinOp op constructor leftParser rightParser = Parser f where
    f tokens = case parse leftParser tokens of
        Right (x:xs, l)
            | x == op && xs == [] -> Right ([], l)
            | x == op             -> parse (fmap (\r -> constructor l r) rightParser) xs
        other                     -> Left $ "Failed to parse " ++ (show op) ++ "."

-----------------------------------------------------------------------
-- Parse plus expression.
-----------------------------------------------------------------------

parsePlus :: Parser Lexeme Expr
parsePlus = Parser f where
    f tokens = case parse parseLeftPlusOperand tokens of
        Right ([Lexer.Plus], l)    -> Right ([], l)
        Right ((Lexer.Plus):xs, l) -> parse (fmap (\r -> SyntaxAnalyzer2.Plus l r) parseRightPlusOperand) xs
        other                      -> Left "Failed parse +."

--parsePlus = parseBinOp Lexer.Plus SyntaxAnalyzer2.Plus parseLeftPlusOperand parseRightPlusOperand

parseLeftPlusOperand :: Parser Lexeme Expr
parseLeftPlusOperand = parseMul <|> parseDiv <|> parseBraces <|> parseSingleNumber

parseRightPlusOperand :: Parser Lexeme Expr
parseRightPlusOperand = parseExpr

-----------------------------------------------------------------------
-- Parse minus expression.
-----------------------------------------------------------------------

parseMinus :: Parser Lexeme Expr
parseMinus = parseBinOp Lexer.Minus SyntaxAnalyzer2.Minus parseLeftMinusOperand parseRightMinusOperand

parseLeftMinusOperand :: Parser Lexeme Expr
parseLeftMinusOperand = parseMul <|> parseDiv <|> parseBraces <|> parseSingleNumber

parseRightMinusOperand :: Parser Lexeme Expr
parseRightMinusOperand = parseExpr

-----------------------------------------------------------------------
-- Parse multiply expression.
-----------------------------------------------------------------------

parseMul :: Parser Lexeme Expr
parseMul = parseBinOp Lexer.Mul SyntaxAnalyzer2.Mul parseLeftMulOperand parseRightMulOperand

parseMulOperation :: Parser Lexeme (Maybe Lexeme)
parseMulOperation = satisfyOrEnd isMulLexeme "Failed to parse minus."

parseLeftMulOperand :: Parser Lexeme Expr
parseLeftMulOperand = parseBraces <|> parseSingleNumber

parseRightMulOperand :: Parser Lexeme Expr
parseRightMulOperand = parseMul <|> parseDiv <|> parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse divide expression.
-----------------------------------------------------------------------

parseDiv :: Parser Lexeme Expr
parseDiv = parseBinOp Lexer.Div SyntaxAnalyzer2.Div parseLeftDivOperand parseRightDivOperand

parseDivOperation :: Parser Lexeme (Maybe Lexeme)
parseDivOperation = satisfyOrEnd isDivLexeme "Failed to parse minus."

parseLeftDivOperand :: Parser Lexeme Expr
parseLeftDivOperand = parseBraces <|> parseSingleNumber

parseRightDivOperand :: Parser Lexeme Expr
parseRightDivOperand = parseRightMulOperand

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
parseSingleNumber = fmap (\(Lexer.Number n) -> SyntaxAnalyzer2.Number n) $ satisfy isNumberLexeme "Failed to parse number."
