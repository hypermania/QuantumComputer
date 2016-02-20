{-|
Module      : Parser
Description : Parser for a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Parsing formatted input for the quantum computer
-}

module Parser where

import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Monad.State

import Computer

import qualified Data.Vector as Vec
import Data.Complex
import Data.Char


---------------------------------------------
-- Reading Coefficients


type Coeff = Complex Double 

(<++>) :: ReadP [a] -> ReadP [a] -> ReadP [a]
p1 <++> p2 = (++) <$> p1 <*> p2

digits :: ReadP String
digits = munch1 isDigit

parenth :: ReadP String -> ReadP String
parenth p = string "(" <++> p <++> string ")"

double :: ReadP Double
double = read <$> (doubform +++ parenth doubform)
  where doubform = (noDec <++> string "." <++> digits) <++ noDec
        noDec = option "" (string "-") <++> digits

coeff :: ReadP Coeff
coeff = realAndComplex <++ realOnly
  where realAndComplex = (:+) <$> double <*> (string ":+" >> double)
        realOnly = (:+0) <$> double 

coeffList :: ReadP [Coeff]
coeffList = between (char '[') (char ']') $ sepBy coeff (char ',')

coeffMat :: ReadP [[Coeff]]
coeffMat = between (char '[') (char ']') $ sepBy coeffList (char ',')

stateFromList :: ReadP QState
stateFromList  = Vec.fromList <$> coeffList

onbFromList :: ReadP ONB
onbFromList = (map Vec.fromList) <$> coeffMat

opFromList :: ReadP QOperator
opFromList = transposeOp <$> Vec.fromList <$> (map Vec.fromList) <$> coeffMat

spectFrom :: ReadP SpectralDecom
spectFrom = Vec.fromList <$> between (char '[') (char ']') eigenSpaces
  where eigenSpace = (,) <$> double <*> (string ":" >> onbFromList)
        eigenSpaces = sepBy eigenSpace (char ',')

----------------------------------------
-- Slightly more complicated definitions



----------------------------------------
-- Tokens

type Name = String
type QTypeName = String
type Token = (Name, QTypeName, String)

token :: ReadP Token
token = liftM3 (,,) (munch1 isAlphaNum) (string "=" >> munch1 isAlpha) (string ":" >> munch1 (not . (==';'))) 

readDefs :: ReadP [Token]
readDefs = string "#Definitions:" >> endBy token (string ";")
