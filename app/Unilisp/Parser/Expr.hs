module Unilisp.Parser.Expr where

import Control.Monad

import Data.Char (isAlphaNum)
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
        <|> parseInfixOp
        <|> parseFuncCall

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
    rest  <- manyTill alphaNumChar (lookAhead nonAlphaNumChar)

    return (first : rest)

nonAlphaNumChar :: Parser () 
nonAlphaNumChar = void (satisfy (not . isAlphaNum))

parseVarExpr :: Parser Expr 
parseVarExpr = VarConst <$> parseIdentifier

parseList :: Parser Expr
parseList = do 
    char '['
    es <- sepBy parseExpr (space *> char ',' *> space)
    char ']'

    return (ListConst es)

parseFuncCall :: Parser Expr
parseFuncCall = do 
    char '('
    name  <- space *> parseIdentifier <* space
    args  <- sepBy parseExpr space
    char ')'

    return $ FuncCallConst (FuncCall name args)

parseInfixOp :: Parser Expr 
parseInfixOp = do 
    char '(' *> space
    char '[' *> space
    op <- some (oneOf "`!@#$%^&*-+=\\|:;\"'<>,./?") <* space
    char ']' *> space

    e1 <- parseExpr <* space 
    e2 <- parseExpr <* space

    char ')'

    return (InfixOp e1 op e2)
    
    
