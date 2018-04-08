-----------------------------------------------------------------------
-- Lexer for arithmetic expressions.
--
-- Usage: runLexer on String - arithmetic expression.
-- It will return List of Lexemes.
-----------------------------------------------------------------------

module Lexer where

import Parser
import Data.Char
import Control.Applicative

data Lexeme = Number Int | OpenBrace | CloseBrace | Plus | Minus | Mul | Div | Pow
    deriving (Show, Eq)

-----------------------------------------------------------------------
-- Parse Lexemes
-----------------------------------------------------------------------

-- Main lexer interface.
-- Transforms given string to List of Lexemes.
-- Report an error if given string is not a correct expression.
runLexer :: String -> [Lexeme]
runLexer str = case parse parseLexemes (clearString str) of
    Right ("", [])      -> error "Empty expression."
    Right ("", result)  -> result
    Right _             -> error "Failed to parse all input."
    Left  err           -> error err

-- Parser for all Lexemes.
parseLexemes :: Parser Char [Lexeme]
parseLexemes = many (parseNumber <|> parseBrace <|> parseOperation)

-- Removes all space characters.
clearString :: String -> String
clearString = filter (\c -> not $ elem c " \t")

-----------------------------------------------------------------------
-- Parse Number
-----------------------------------------------------------------------

-- Parser for single digit.
parseDigit :: Parser Char Int
parseDigit = digitToInt <$> satisfy isDigit "Failed to parse digit."

-- Folds list as decimal number.
listToDecimal :: [Int] -> Int
listToDecimal xs = foldl (\a b -> a * 10 + b) 0 xs

-- Parser for natural Numbers.
parseNumber :: Parser Char Lexeme
parseNumber = fmap (Number . listToDecimal) (some parseDigit)

-----------------------------------------------------------------------
-- Parse Brace
-----------------------------------------------------------------------

-- Parser for braces.
parseBrace :: Parser Char Lexeme
parseBrace = fmap getBraceType (satisfy (\c -> elem c "()") "Failed to parse Brace.")

-- Returns brace type OpenBrace '(' or CloseBrace ')'.
getBraceType :: Char -> Lexeme
getBraceType brace = if brace == '(' then OpenBrace else CloseBrace

-----------------------------------------------------------------------
-- Parse operation
-----------------------------------------------------------------------

-- Parser for operations.
parseOperation :: Parser Char Lexeme
parseOperation = fmap getOperationType (satisfy (\c -> elem c "+-*/^") "Failed to parse Operation.")

-- Returns operation type (+, -, *, /, ^).
getOperationType :: Char -> Lexeme
getOperationType op = case op of
    '+' -> Plus
    '-' -> Minus
    '*' -> Mul
    '/' -> Div
    '^' -> Pow

-- Checks if Lexeme is a Number.
isNumberLexeme :: Lexeme -> Bool
isNumberLexeme (Lexer.Number _) = True
isNumberLexeme _                = False

-- Checks if Lexeme is a Plus.
isPlusLexeme :: Lexeme -> Bool
isPlusLexeme Plus = True
isPlusLexeme _  = False

-- Checks if Lexeme is a Minus.
isMinusLexeme :: Lexeme -> Bool
isMinusLexeme Minus = True
isMinusLexeme _  = False

-- Checks if Lexeme is a Mul.
isMulLexeme :: Lexeme -> Bool
isMulLexeme Mul = True
isMulLexeme _  = False

-- Checks if Lexeme is a Div.
isDivLexeme :: Lexeme -> Bool
isDivLexeme Div = True
isDivLexeme _  = False

-- Checks if Lexeme is a Pow.
isPowLexeme :: Lexeme -> Bool
isPowLexeme Pow = True
isPowLexeme _  = False

-- Checks if Lexeme is a CloseBrace.
isCloseBraceLexeme :: Lexeme -> Bool
isCloseBraceLexeme CloseBrace = True
isCloseBraceLexeme _  = False

-- Checks if Lexeme is a OpenBrace.
isOpenBraceLexeme :: Lexeme -> Bool
isOpenBraceLexeme OpenBrace = True
isOpenBraceLexeme _  = False

