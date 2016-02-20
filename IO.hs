{-|
Module      : IO
Description : IO for a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Defines input/output for the quantum computer
-}

module IO where

import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Monad.State
import System.IO
import Computer
import Data.Complex
import Data.Char


---------------------------------------------
-- 

type Coeff = ReadP (Complex Double)

digit = satisfy isDigit

int = many1 digit

double = (++) <$> int <*> int

double' :: ReadP Double
double' = read <$> double
