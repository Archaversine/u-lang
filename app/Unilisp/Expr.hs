module Unilisp.Expr where

import Data.String (IsString(..))

import GHC.Real ( Ratio((:%)) )

type Function = String

data Expr = BoolConst     !Bool 
          | IntConst      !Integer
          | DoubleConst   !Double
          | StringConst   !String
          | VarConst      !String
          | ListConst     ![Expr]
          | FuncCallConst !FunctionCall
          | ErrorValue    !String
          deriving (Eq, Show)

data FunctionCall = FuncCall !Function ![Expr] deriving (Eq, Show)

set :: String -> Expr
set = VarConst

instance IsString Expr where 
    fromString = StringConst

instance Num Expr where
    _ + _ = ErrorValue "Not implemented"
    _ - _ = ErrorValue "Not implemented"
    _ * _ = ErrorValue "Not implemented"

    negate = const $ ErrorValue "Not implemented"
    abs    = const $ ErrorValue "Not implemented"
    signum = const $ ErrorValue "Not implemented"

    fromInteger = IntConst

instance Fractional Expr where 
    fromRational (a :% b) = DoubleConst (fromInteger a / fromInteger b)
    _ / _ = ErrorValue "Not implemented"
