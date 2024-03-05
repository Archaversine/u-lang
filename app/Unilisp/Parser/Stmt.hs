{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unilisp.Parser.Stmt where

import Unilisp.Expr
import Unilisp.Stmt
import Unilisp.Translate
import Unilisp.Parser.Expr 
import Unilisp.Parser.Types

parseStmt :: Parser Stmt 
parseStmt = try parseFuncDecl 
        <|> try parseVarDecl 
        <|> try parseReturn
        <|> parseFuncStmt

parseFuncStmt :: Parser Stmt
parseFuncStmt = do 
    char '('
    name  <- hspace *> parseIdentifier <* hspace
    args  <- sepBy parseExpr hspace
    char ')'

    return $ FuncCallStmt (FuncCall name args)

parseFuncParams :: Parser [String]
parseFuncParams = do 
    char '['
    ps <- sepBy parseIdentifier space
    char ']'

    return ps

-- (func add [a, b] (+ a b))
parseFuncDecl :: Parser Stmt 
parseFuncDecl = do 
    char '('
    string "func"

    name <- hspace1 *> parseIdentifier
    ps   <- hspace  *> parseFuncParams
    body <- hspace  *> sepBy parseStmt hspace1

    return (FunctionDecl name ps body)

parseVarDecl :: Parser Stmt 
parseVarDecl = do 
    char '('
    string "var"

    name <- hspace1 *> parseIdentifier
    expr <- hspace  *> parseExpr <* char ')'

    return (VarDecl name expr)

parseReturn :: Parser Stmt 
parseReturn = do 
    char '('        *> space
    string "return" *> space

    expr <- parseExpr <* space

    char ')'

    return (ReturnExpr expr)

