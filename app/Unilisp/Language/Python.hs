{-# LANGUAGE TypeApplications #-}

module Unilisp.Language.Python where

import Data.List (intercalate)

import Unilisp.Translate

data Python

instance Language Python where
    mkBool = show

    mkVarDecl name e = name ++ " = " ++ translateExpr @Python e

    mkFuncCall name es = name ++ "(" ++ params ++ ")"
        where params = intercalate ", " $ map (translateExpr @Python) es

    mkFuncDecl name ps ss = "def " ++ name ++ "(" ++ params ++ "):\n" ++ body
        where params = intercalate ", " ps
              body   = unlines $ map (\s -> "\t" ++ translateStmt @Python s) ss
