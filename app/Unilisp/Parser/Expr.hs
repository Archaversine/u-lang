module Unilisp.Parser.Expr where

import Data.Functor (($>))
import Data.Maybe (fromMaybe)

import Unilisp.Expr
import Unilisp.Parser.Types

parseExpr :: Parser Expr
parseExpr = parseBool 
        <|> parseInteger
        <|> parseDouble
        <|> parseString
        <|> parseVarExpr
        <|> parseList

parseBool :: Parser Expr
parseBool = BoolConst <$> choice [ string "True"  $> True 
                                 , string "False" $> False
                                 ]

parseInteger :: Parser Expr 
parseInteger = do 
    sign <- optional (string "-")
    num  <- some digitChar

    let integer = fromMaybe "" sign ++ num

    return $ IntConst (read integer)

parseDouble :: Parser Expr 
parseDouble = do 
    sign  <- optional (string "-")
    first <- some digitChar 
    rest  <- optional (char '.' *> some digitChar)

    let double = fromMaybe "" sign ++ first ++ fromMaybe "" rest

    return $ DoubleConst (read double)

parseString :: Parser Expr 
parseString = StringConst <$> (char '"' *> manyTill asciiChar (char '"'))


parseIdentifier :: Parser String
parseIdentifier = do
    first <- letterChar
    rest  <- some alphaNumChar

    return (first : rest)

parseVarExpr :: Parser Expr 
parseVarExpr = VarConst <$> parseIdentifier

parseList :: Parser Expr
parseList = do 
    char '['
    es <- sepBy parseExpr (hspace *> char ',' *> hspace)
    char ']'

    return (ListConst es)

parseFuncCall :: Parser Expr
parseFuncCall = do 
    char '('
    name  <- hspace *> parseIdentifier <* hspace
    args  <- sepBy parseExpr hspace
    char ')'

    return $ FuncCallConst (FuncCall name args)

