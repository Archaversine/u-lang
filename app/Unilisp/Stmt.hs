module Unilisp.Stmt where

import Data.String (IsString(..))

import Unilisp.Expr

data Stmt = FuncCallStmt !FunctionCall
          | FunctionDecl !Function ![String] ![Stmt]
          | VarDecl      !String   !Expr
          | ReturnExpr   !Expr
          deriving (Eq, Show)
