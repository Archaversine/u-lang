{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

import Text.Megaparsec
import Text.Megaparsec.Char

import Unilisp.Stmt
import Unilisp.Parser.Stmt
import Unilisp.Translate
import Unilisp.Language.Python

parsePython xs = parse (sepBy parseStmt space) "<testing>" (trim xs)
    where trim = dropWhile isSpace . dropWhileEnd isSpace

main :: IO ()
main = do 
    input <- readFile "input.txt"

    case parsePython input of 
        Left e -> putStrLn $ errorBundlePretty e
        Right xs -> writeFile "output.py" (translate @Python xs)
