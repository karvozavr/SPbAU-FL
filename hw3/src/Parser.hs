-----------------------------------------------------------------------
-- Simple applicative parser implementation.
-----------------------------------------------------------------------

module Parser where

import Control.Applicative

newtype Parser tok a = Parser {parse :: [tok] -> Either String ([tok], a)}

instance Functor (Parser tok) where 
	fmap g (Parser p) = Parser $ (fmap . fmap . fmap) g p

instance Applicative (Parser tok) where
    pure x = Parser $ \s -> Right (s, x)
    u <*> v = Parser f where
        f xs = case parse u xs of
			Left err -> Left err
			Right (xs', g) -> case parse v xs' of
				Left err -> Left err
				Right (xs'', x) -> Right (xs'', g x)

instance Alternative (Parser tok) where
	empty = Parser $ \s -> Left "Empty parser element."
	u <|> v = Parser f where
		f xs = case parse u xs of
			Left err -> parse v xs
			result -> result 

-- Satisfy predicate parser.
satisfy :: (tok -> Bool) -> String -> Parser tok tok
satisfy pr errorMsg = Parser f where
	f (c:cs) | pr c = Right (cs, c)
	f _ 		    = Left errorMsg

-- Check predicate parser.
satisfyOrEnd :: (tok -> Bool) -> String -> Parser tok (Maybe tok)
satisfyOrEnd pr errorMsg = Parser f where
	f (c:cs) | pr c = Right (cs, Just c)
	f []            = Right ([], Nothing)
	f _ 		= Left errorMsg

parserError :: String -> Parser a b
parserError errorMsg =  Parser f where
    f _ = Left errorMsg
