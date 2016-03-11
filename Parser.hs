{-|
Module      : Parser
Description : Parser for a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Parsing formatted input for the quantum computer.
-}


module Parser (
  -- * Usage
  -- $usage
  readP_to_S,
  readComputation
  --evalInput
              )
       where

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

type Store a = Map.Map String a

read2Int :: ReadP (Int, Int)
read2Int = char '(' >> ((,) <$> int <*> (char ',' >> int)) <* char ')'

read3Int :: ReadP (Int, Int, Int)
read3Int = char '(' >> (liftM3 (,,) int (char ',' >> int) (char ',' >> int)) <* char ')'

name :: ReadP String
name = munch1 isAlphaNum

read2Name :: ReadP (String, String)
read2Name = char '(' >> ((,) <$> name <*> (char ',' >> name)) <* char ')'

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

stateFormats :: [ReadP QState]
stateFormats =
  [string "Vec" >> stateFromList,
   string "Int" >> (uncurry intV) <$> read2Int,
   string "Zero" >> initV <$> int,
   string "Superposed" >> superposedIntV <$> int--,
--   string "TensorProdV" >> (uncurry tensorProdV) <$> read2Name
  ]

readState :: ReadP (String, QState)
readState = (,) <$> name <*> (string "=" >> choice stateFormats)

readStates :: ReadP (Store QState)
readStates = Map.fromList <$> endBy readState (char ';')

opFormats :: [ReadP QOperator]
opFormats =
  [string "Mat" >> opFromList,
   string "FullHadamard" >> hadamardOpFull <$> int,
   string "BitwiseHadamard" >> (uncurry hadamardOpAt) <$> read2Int,
   string "BitwiseNOT" >> (uncurry pauliXAt) <$> read2Int,
   string "BitwisePhase" >> phaseOf <$> double,
   string "BitwiseS" >> return phaseS,
   string "BitwiseT" >> return phaseT,
   string "BitwiseSAt" >> (uncurry phaseSAt) <$> read2Int,
   string "BitwiseTAt" >> (uncurry phaseTAt) <$> read2Int,
   string "CNOT" >> (uncurry3 cnotAt) <$> read3Int,
   string "ControlledT" >> (uncurry3 cTAt) <$> read3Int,
   string "ControlledS" >> (uncurry3 cSAt) <$> read3Int,
   string "QFT" >> (uncurry3 qftBitNaive) <$> read3Int,
   string "InverseQFT" >> (uncurry3 iqftBitNaive) <$> read3Int,
   string "MultModN" >> (uncurry3 multModN) <$> read3Int
  ]

readOp :: ReadP (String, QOperator)
readOp = (,) <$> name <*> (string "=" >> choice opFormats)

readOps :: ReadP (Store QOperator)
readOps = Map.fromList <$> endBy readOp (char ';')

spectFormats :: [ReadP SpectralDecom]
spectFormats =
  [string "EigenSys" >> spectFromList,
   string "CompBasis" >> (uncurry3 compBasisSpect) <$> read3Int
  ]

readSpect :: ReadP (String, SpectralDecom)
readSpect = (,) <$> name <*> (string "=" >> choice spectFormats)

readSpects :: ReadP (Store SpectralDecom)
readSpects = Map.fromList <$> endBy readSpect (char ';')

-------------------------------------
-- Reading commands

readStateVar :: Store QState -> ReadP (String, QState)
readStateVar vecs = gather $
                    choice stateFormats <++ ((vecs Map.!) <$> name)

readOpVar :: Store QOperator -> ReadP (String, QOperator)
readOpVar ops = gather $
                choice opFormats <++ ((ops Map.!) <$> name)

readSpectVar :: Store SpectralDecom -> ReadP (String, SpectralDecom)
readSpectVar spects = gather $
                      choice spectFormats <++ ((spects Map.!) <$> name)


readCommand :: Store QState -> Store QOperator -> Store SpectralDecom
            -> ReadP (QComputer QCommand)
readCommand vecs ops spects = choice actionType
  where actionType =
          [string "Apply:"
           >> (\(name, op) -> applyGate name op) <$> readOpVar ops,
           string "InitializeTo:"
           >> (\(name, vec) -> initialize name vec) <$> readStateVar vecs,
           string "SpectMeasure:"
           >> (\(name, spect) -> spectMeasure name spect) <$> readSpectVar spects,
           string "NumMeasure" >> return numberMeasure
          ]

readCommands :: Store QState -> Store QOperator -> Store SpectralDecom
             -> ReadP [QComputer QCommand]
readCommands vecs ops spects = endBy (readCommand vecs ops spects) (char ';')

-- | Reading a complete file.
readComputation :: ReadP [QComputer QCommand]
readComputation = do
  string "#States:"
  vecs <- (char '{' >> readStates) <* char '}'
  string "#Operators:"
  ops <- char '{' >> readOps <* char '}'
  string "#SpectralDecoms:"
  spects <- char '{' >> readSpects <* char '}'
  string "#Commands:"
  cmds <- char '{' >> (readCommands vecs ops spects) <* char '}'
  return cmds

{- $usage

The computation here

-}
