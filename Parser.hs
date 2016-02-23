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
import Control.Monad.Random
import Computer

import qualified Data.Vector as Vec
import Data.Complex
import Data.Char
import qualified Data.Map as Map

---------------------------------------------
-- Reading Coefficients

type Coeff = Complex Double 

(<++>) :: ReadP [a] -> ReadP [a] -> ReadP [a]
p1 <++> p2 = (++) <$> p1 <*> p2

digits :: ReadP String
digits = munch1 isDigit

parenth :: ReadP String -> ReadP String
parenth p = string "(" <++> p <++> string ")"

int :: ReadP Int
int = read <$> (intForm +++ parenth intForm)
  where intForm = option "" (string "-") <++> digits

double :: ReadP Double
double = read <$> (doubForm +++ parenth doubForm)
  where doubForm = (noDec <++> string "." <++> digits) <++ noDec
        noDec = option "" (string "-") <++> digits

coeff :: ReadP Coeff
coeff = realAndComplex <++ realOnly
  where realAndComplex = (:+) <$> double <*> (string ":+" >> double)
        realOnly = (:+0) <$> double 

--------------------------------------------
-- Reading vectors/operators

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

spectFromList :: ReadP SpectralDecom
spectFromList = Vec.fromList <$> between (char '[') (char ']') eigenSpaces
  where eigenSpace = (,) <$> double <*> (string ":" >> onbFromList)
        eigenSpaces = sepBy eigenSpace (char ',')

----------------------------------------
-- Reading definitions

type Name = String
type Store a = Map.Map Name a

read2Int :: ReadP (Int, Int)
read2Int = char '(' >> ((,) <$> int <*> (char ',' >> int)) <* char ')'

read3Int :: ReadP (Int, Int, Int)
read3Int = char '(' >> (liftM3 (,,) int (char ',' >> int) (char ',' >> int)) <* char ')'

readState :: ReadP (Name, QState)
readState = (,) <$> (munch1 isAlphaNum) <*> (string "=" >> choice stateFormats)
  where stateFormats =
          [string "Vec:" >> stateFromList,
           string "VecInt:" >> (uncurry intV) <$> read2Int
          ]

readStates :: ReadP (Store QState)
readStates = Map.fromList <$> endBy readState (char ';')

readOp :: ReadP (Name, QOperator)
readOp = (,) <$> (munch1 isAlphaNum) <*> (string "=" >> choice opFormats)
  where opFormats =
          [string "Mat:" >> opFromList,
           string "FullHadamard:" >> hadamardOpFull <$> int,
           string "BitwiseHadamard:" >> (uncurry hadamardOpAt) <$> read2Int
          ]

readOps :: ReadP (Store QOperator)
readOps = Map.fromList <$> endBy readOp (char ';')

readSpect :: ReadP (Name, SpectralDecom)
readSpect = (,) <$> (munch1 isAlphaNum) <*> (string "=" >> choice spectFormats)
  where spectFormats =
          [string "EigenSys:" >> spectFromList
          ]

readSpects :: ReadP (Store SpectralDecom)
readSpects = Map.fromList <$> endBy readSpect (char ';')

-------------------------------------
-- Reading commands

readCommand :: Store QState -> Store QOperator -> Store SpectralDecom
            -> ReadP (QComputer QCommand)
readCommand vecs ops spects = choice actionType
  where actionType =
          [string "Apply:"
           >> (\name -> applyGate (ops Map.! name)) <$> munch1 isAlphaNum,
           string "InitializeTo:"
           >> (\name -> initialize (vecs Map.! name)) <$> munch1 isAlphaNum
          ]

readCommands :: Store QState -> Store QOperator -> Store SpectralDecom
             -> ReadP [QComputer QCommand]
readCommands vecs ops spects = endBy (readCommand vecs ops spects) (char ';')


-------------------------------------
-- Reading a complete file

readComputation :: ReadP (QComputer [QCommand])
readComputation = do
  string "#States:"
  vecs <- (char '{' >> readStates) <* char '}'
  string "#Operators:"
  ops <- char '{' >> readOps <* char '}'
  string "#SpectralDecoms:"
  spects <- char '{' >> readSpects <* char '}'
  string "#Commands:"
  cmds <- char '{' >> (readCommands vecs ops spects) <* char '}'
  return $ Control.Monad.sequence cmds

runInput :: StdGen -> String -> Either String [QCommand]
runInput g input = if length parsed > 0
                   then evalQC g $ fst . last $ parsed
                   else Left "No parse"
  where parsed = readP_to_S readComputation input

