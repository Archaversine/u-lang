module Unilisp.Parser.Types ( Parser 
                            , module Text.Megaparsec 
                            , module Text.Megaparsec.Char 
                            ) where

import Data.Void

import Text.Megaparsec 
import Text.Megaparsec.Char

type Parser = Parsec Void String
