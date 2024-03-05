module Unilisp.Stmt where

import Data.String (IsString(..))

import Unilisp.Expr

data Stmt = FuncCallStmt !FunctionCall
          | FunctionDecl !Function ![String] ![Stmt]
          | VarDecl      !String   !Expr
          deriving (Eq, Show)
