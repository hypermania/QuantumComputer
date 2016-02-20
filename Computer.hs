{-|
Module      : Quantum.Computer
Description : Implements basic elements of a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Defines basic elementss of a quantum computer, including representation of the state vector and a state monad for evolution of quantum state.
-}
module Quantum.Computer where

import Data.List as List
import Data.Vector as Vec
import Data.Complex
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Applicative

------------------------------------------------------------
-- Basic vector operations

-- | Type for the quantum state
-- Indexing of the state vectors usually go from 0 to 2^n-1,
-- where n is the number of qubits
type QState = Vector (Complex Double)

-- | Type for an orthogonal normalized LI set of state vectors
type ONB = [QState]

-- | Vector addition
addV :: QState -> QState -> QState
addV = Vec.zipWith (+)

-- | Scalar multiplication
scalV :: Complex Double -> QState -> QState
scalV k = Vec.map (*k)

-- | State norm
normV :: QState -> Double
normV = sqrt . Vec.sum . Vec.map (\c -> (magnitude c)^2)

-- | Normalize state
normalizeV :: QState -> QState
normalizeV psi = Vec.map (/norm) psi
  where norm = normV psi :+ 0

-- | Inner product
innerProdV :: QState -> QState -> Complex Double
innerProdV psi phi = Vec.sum $ Vec.zipWith (\c1 c2 -> conjugate c1 * c2) psi phi

-- | Tensor product
tensorProdV :: QState -> QState -> QState
tensorProdV psi phi = Vec.concatMap (`scalV` phi) psi

-- | Projection to the space spanned by an ONB
-- ONB must contain at least one vector
projectTo :: ONB -> QState -> QState
projectTo [] psi = Vec.replicate (Vec.length psi) (0:+0)
projectTo p psi = List.foldr1 addV (List.map (\v -> (v `innerProdV` psi) `scalV` v) p)

--------------------------------------------------------
-- Commonly used vectors

-- | Number of qubits
type Qubits = Int

-- | The zero vector
zeroV :: Qubits -> QState
zeroV n = Vec.replicate (2^n) (0:+0)

-- | The vector that represents |n>
-- bits denote the number of qubits used
-- the input should satisfy n<2^bits
intV :: Qubits -> Int -> QState
intV bits n = (Vec.replicate n (0:+0)) Vec.++ Vec.singleton (1:+0) Vec.++ (Vec.replicate (2^bits-(n+1)) (0:+0))

-- | The vector that represents the zero number |00..0>
-- not to be confused with the zero vector (zeroV)
initV :: Qubits -> QState
initV bits = intV bits 0

-- | The trivial superposition of all numbers |n>  (n<2^bits)
superposedIntV :: Qubits -> QState
superposedIntV bits = Vec.replicate (2^bits) (comp:+0)
  where comp = 1 / sqrt (2^bits)

-------------------------------------------------------
-- Basic operator operations

-- | Represent a list of column vectors
type QOperator = Vector QState

-- | Represent a list of (eigenvalue,eigenstate) pair
type SpectralDecom = Vector (Double, ONB)

-- | Let an operator act on a state
actOn :: QOperator -> QState -> QState
actOn op psi = Vec.foldr1 addV $ Vec.zipWith (flip scalV) op psi

addOp :: QOperator -> QOperator -> QOperator
addOp = Vec.zipWith addV

scalOp :: Complex Double -> QOperator -> QOperator
scalOp k = (<$>) (k `scalV`) 

multOp :: QOperator -> QOperator -> QOperator
multOp op = Vec.map (op `actOn`)

-- | Transpose of an operator
transposeOp :: QOperator -> QOperator
transposeOp op = Vec.map (\n -> Vec.map (!n) op) $ Vec.enumFromTo 0 (rows-1)
  where rows = Vec.length $ op!0

-- | Complex conjugate of an operator
conjugateOp :: QOperator -> QOperator
conjugateOp = (fmap . fmap) conjugate

-- | The hermitian conjugate
hConjugateOp :: QOperator -> QOperator
hConjugateOp = transposeOp . conjugateOp

commutatorOp :: QOperator -> QOperator -> QOperator
commutatorOp op1 op2 = (op1 `multOp` op2) `addOp` (scalOp (-1) (op2 `multOp` op1))

isHermitian :: QOperator -> Bool
isHermitian op = hConjugateOp op == op

isUnitary :: QOperator -> Bool
isUnitary op = isSquare && isNormalized && isOrthogonal
  where
    isSquare = rows == columns
    isNormalized = Vec.and . Vec.map ((==1.0) . normV) $ op
    isOrthogonal = Vec.and . Vec.map (\c -> Vec.and . Vec.map ((==0.0) . (op!c `innerProdV`)) $ Vec.drop (c+1) op) $ Vec.enumFromTo 0 (columns-1)
    rows = Vec.length $ op!0
    columns = Vec.length op

spectralDecom :: QOperator -> SpectralDecom
spectralDecom = undefined

------------------------------------------------------
-- Common operators

identityOp :: Qubits -> QOperator
identityOp bits = Vec.map (intV bits) $ Vec.enumFromTo 0 (2^bits-1)

pauli_I, pauli_X, pauli_Y, pauli_Z :: QOperator
pauli_I = identityOp 1
pauli_X = Vec.fromList [Vec.fromList [0,1], Vec.fromList [1,0]]
pauli_Y = Vec.fromList [Vec.fromList [0,0:+1], Vec.fromList [0:+(-1),0]]
pauli_Z = Vec.fromList [Vec.fromList [1,0], Vec.fromList [0,-1]]


hadamardRec :: QOperator -> QOperator
hadamardRec op = scalOp (1/(sqrt 2.0)) hmat
  where
    negop = scalOp (negate 1) op
    hmat = (Vec.zipWith (Vec.++) op op) Vec.++ (Vec.zipWith (Vec.++) op negop)

hadamardOp :: Qubits -> QOperator
hadamardOp 0 = identityOp 0
hadamardOp bits = hadamardRec $ hadamardOp (bits-1)

pauli_X_decom :: SpectralDecom
pauli_X_decom = Vec.fromList [(1.0,[Vec.fromList [1:+0,1:+0]]),(-1.0,[Vec.fromList [1:+0,(-1):+0]])]

-------------------------------------------------------
-- Quantum state monads

-- | The state monad representing the quantum computer QState 
-- is the quantum state of the computer, StdGen is a random
-- number generator that is used at quantum measurements
type QComputer = State (QState, StdGen)

type QMeasure = QComputer Double

randFromDist :: Vector (a,Double) -> QComputer a
randFromDist xs
  | Vec.length xs == 0 = error "randFromDist called from empty vector"
  | Vec.length xs == 1 = return . fst . Vec.head $ xs
  | otherwise = do
      (psi,g) <- get
      let
        s = Vec.sum (Vec.map snd xs)
        cs = Vec.scanl1 (\(_,summed) (num,nextProb) -> (num,summed+nextProb)) xs
      let (p,g') = randomR (0.0,s) g
      put (psi,g')
      return . fst . Vec.head $ Vec.dropWhile (\(_,q) -> q<p) cs

measure :: SpectralDecom -> QMeasure
measure op = do
  (psi,_) <- get
  let
    probs = Vec.map (\(eigval, eigvecs) -> (eigval, (^2) . magnitude . List.sum . List.map (`innerProdV` psi) $ eigvecs)) op
  result <- randFromDist probs
  (_,g) <- get
  put (normalizeV $ projectTo (snd . Vec.head $ Vec.filter (\(ev,_) -> ev==result) op) psi, g)
  return result

{-
compute :: QMeasure
compute = do
  measure pauli_Y
  measure pauli_Z
  measure pauli_Z
  measure pauli_Z
-}