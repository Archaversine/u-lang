{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unilisp.Translate where 

import Data.Char (toLower)

import Unilisp.Stmt
import Unilisp.Expr
import Unilisp.Parser.Types

class Language lang where 
    mangleFunc :: Function -> Function
    mangleFunc = id

    mkBool :: Bool -> String
    mkBool = map toLower . show

    mkInt :: Integer -> String 
    mkInt = show

    mkDouble :: Double -> String
    mkDouble = show

    mkVarName :: String -> String
    mkVarName = id

    mkString :: String -> String
    mkString = show

    mkList :: [Expr] -> String
    mkList = show

    mkVarDecl  :: String   -> Expr     -> String
    mkFuncCall :: Function -> [Expr  ] -> String
    mkFuncDecl :: Function -> [String] -> [Stmt] -> String

    translateExpr :: Expr -> String
    translateExpr = \case
        BoolConst     x -> mkBool     @lang x
        IntConst      x -> mkInt      @lang x
        DoubleConst   x -> mkDouble   @lang x
        StringConst   x -> mkString   @lang x
        VarConst      x -> mkVarName  @lang x
        ListConst     x -> mkList     @lang x
        FuncCallConst x -> mkFuncCall @lang name ps where FuncCall name ps = x
        ErrorValue    x -> errorWithoutStackTrace $ "Parsing Error: " ++ x

    translateStmt :: Stmt -> String
    translateStmt = \case 
        FuncCallStmt (FuncCall f ps) -> mkFuncCall @lang f ps
        FunctionDecl f ps b          -> mkFuncDecl @lang f ps b
        VarDecl      v ex            -> mkVarDecl  @lang v ex

    translate :: [Stmt] -> String
    translate = unlines . map (translateStmt @lang)
