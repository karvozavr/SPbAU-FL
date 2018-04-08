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
    | Deg Expr Expr
    | Number Int
    deriving (Show, Eq)

-- Main analyzer interface.
-- Transforms given string to AST.
-- Report an error if given string is not a correct arithmetic expression.
analyzeExpression :: String -> Expr
analyzeExpression str = buildAST $ runLexer str

buildAST :: [Lexeme] -> Expr
buildAST tokens = case parse parseExpr tokens of
    Right ([], result)   -> result
    Right (rest, result) -> error ("Failed failed to parse till the end of the line." ++ (show rest))
    Left  err            -> error err


-- Parser for expression.
parseExpr :: Parser Lexeme Expr
parseExpr = parsePlusMinus <|> parseMulDiv <|> parseSingleNumber <|> parseBraces

-- Optimizes binary operation parsing:
-- if failed to parse operation because of the end of line, return left operand as parsed expression.
optimizeBinOp :: (Expr -> Expr -> Expr) -> Parser Lexeme Expr -> Parser Lexeme (Maybe Lexeme -> Expr -> Expr)
optimizeBinOp constructor = fmap (optimizeBinOpHelper constructor)

optimizeBinOpHelper :: (Expr -> Expr -> Expr) -> Expr -> Maybe Lexeme -> Expr -> Expr
optimizeBinOpHelper constructor a operation b | operation == Nothing = a
                                              | otherwise            = constructor a b

--lexemeToExprConstructor :: Lexeme -> (Expr -> Expr -> Expr)
--lexemeToExprConstructor lexeme = case lexeme of
--    Lexer.Plus  -> SyntaxAnalyzer.Plus
--    Lexer.Minus -> SyntaxAnalyzer.Minus
--    Lexer.Mul   -> SyntaxAnalyzer.Mul
--    Lexer.Div   -> SyntaxAnalyzer.Div
--    Lexer.Deg   -> SyntaxAnalyzer.Deg


-----------------------------------------------------------------------
-- Parse plus expression.
-----------------------------------------------------------------------

--parsePlus :: Parser Lexeme Expr
--parsePlus = (optimizeBinOp SyntaxAnalyzer.Plus parseLeftPlusOperand) <*> parsePlusOperation <*> parseRightPlusOperand
--
--parsePlusOperation :: Parser Lexeme (Maybe Lexeme)
--parsePlusOperation = satisfyOrEnd isPlusLexeme "Failed to parse plus."
--
--parseLeftPlusOperand :: Parser Lexeme Expr
--parseLeftPlusOperand = parseMul <|> parseDiv <|> parseBraces <|> parseSingleNumber
--
--parseRightPlusOperand :: Parser Lexeme Expr
--parseRightPlusOperand = parseExpr

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
parsePlusMinusOperand = parseMulDiv <|> parseBraces <|> parseSingleNumber

-----------------------------------------------------------------------
-- Parse multiply, divide priority expression.
-----------------------------------------------------------------------

parseMulDiv :: Parser Lexeme Expr
parseMulDiv = Parser f where
    f tokens = case parse parseMulDivOperand tokens of
        Right (ts, result) -> parsePlusMinusHelper result ts
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
parseMulDivOperand = parseBraces <|> parseSingleNumber


-----------------------------------------------------------------------
-- Parse multiply expression.
-----------------------------------------------------------------------

--parseMul :: Parser Lexeme Expr
--parseMul = (optimizeBinOp SyntaxAnalyzer.Mul parseLeftMulOperand) <*> parseMulOperation <*> parseRightMulOperand
--
--parseMulOperation :: Parser Lexeme (Maybe Lexeme)
--parseMulOperation = satisfyOrEnd isMulLexeme "Failed to parse multiplication."
--
--parseLeftMulOperand :: Parser Lexeme Expr
--parseLeftMulOperand = parseBraces <|> parseSingleNumber
--
--parseRightMulOperand :: Parser Lexeme Expr
--parseRightMulOperand = parseMul <|> parseDiv <|> parseBraces <|> parseSingleNumber
--
-------------------------------------------------------------------------
---- Parse divide expression.
-------------------------------------------------------------------------
--
--parseDiv :: Parser Lexeme Expr
--parseDiv = (optimizeBinOp SyntaxAnalyzer.Div parseLeftDivOperand) <*> parseDivOperation <*> parseRightDivOperand
--
--parseDivOperation :: Parser Lexeme (Maybe Lexeme)
--parseDivOperation = satisfyOrEnd isDivLexeme "Failed to parse division."
--
--parseLeftDivOperand :: Parser Lexeme Expr
--parseLeftDivOperand = parseBraces <|> parseSingleNumber
--
--parseRightDivOperand :: Parser Lexeme Expr
--parseRightDivOperand = parseRightMulOperand

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
