{-|
Module      : Computer
Description : Implements basic elements of a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

Defines basic elements of a quantum computer, including representation of the state vector, state operators, and a state monad for evolution of quantum state.
-}
module Computer
       (
         QState,
         ONB,
         Qubits,
         QOperator,
         SpectralDecom,
         QComputer,
         transposeOp,
         intV,
         superposedIntV,
         hadamardOpAt,
         hadamardOpFull,
         actOn,
         applyGate
       )
       where

import qualified Data.List as List
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
normV = sqrt . Vec.sum . Vec.map (\c -> magnitude c ^ 2)

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

-- | Number/Position of qubits
type Qubits = Int

-- | The zero vector
zeroV :: Qubits -> QState
zeroV n = Vec.replicate (2^n) (0:+0)

-- | The vector that represents |n>
-- bits denote the number of qubits used
-- the input should satisfy n<2^bits
intV :: Qubits -> Int -> QState
intV bits n = Vec.replicate n (0:+0) Vec.++ Vec.singleton (1:+0) Vec.++ Vec.replicate (2^bits-(n+1)) (0:+0)

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

-- | Tensor product for an operator
tensorProdOp :: QOperator -> QOperator -> QOperator
tensorProdOp a b = Vec.map (\k -> join $ Vec.map (`scalV` (b Vec.! (k `mod` q))) (a Vec.! (k `div` q))) $ Vec.enumFromTo 0 (p*q-1)
  where p = Vec.length a
        q = Vec.length b

commutatorOp :: QOperator -> QOperator -> QOperator
commutatorOp op1 op2 = (op1 `multOp` op2) `addOp` scalOp (-1) (op2 `multOp` op1)

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
-- Common single-bit operators
-- (including single-bit operators for multi-bit systems

-- | 1x1 matrix containt 1, useful for defining other operators
baseOp :: QOperator
baseOp = Vec.fromList [Vec.fromList [1]]

pauliI, pauliX, pauliY, pauliZ :: QOperator
pauliI = Vec.fromList [Vec.fromList [1,0], Vec.fromList [0,1]] --Identity
pauliX = Vec.fromList [Vec.fromList [0,1], Vec.fromList [1,0]] --NOT gate
pauliY = Vec.fromList [Vec.fromList [0,0:+1], Vec.fromList [0:+(-1),0]]
pauliZ = Vec.fromList [Vec.fromList [1,0], Vec.fromList [0,-1]]

-- | Input the total number of qubits (bits) and an index (i) for a qubit
-- give the operator that performs gate (op) on qubit i, while
-- keeping the other qubits fixed
bitOpAt :: Qubits -> Qubits -> QOperator -> QOperator
bitOpAt bits i op = before `tensorProdOp` op `tensorProdOp` after
  where before = iterate (`tensorProdOp` pauliI) baseOp !! (i-1)
        after = iterate (`tensorProdOp` pauliI) baseOp !! (bits-i)

identityOp :: Qubits -> QOperator
identityOp bits = iterate (`tensorProdOp` pauliI) baseOp !! bits

hadamardBase :: QOperator
hadamardBase = (1/sqrt 2.0) `scalOp` Vec.fromList [Vec.fromList [1,1], Vec.fromList [1,-1]]

hadamardOpFull :: Qubits -> QOperator
hadamardOpFull bits = iterate (`tensorProdOp` hadamardBase) baseOp !! bits

hadamardOpAt :: Qubits -> Qubits -> QOperator
hadamardOpAt bits i = bitOpAt bits i hadamardBase

-- | Input the total number of qubits (n) and an index (i) for a qubit
-- give an operator that performs NOT gate on qubit i
pauliXAt :: Qubits -> Qubits -> QOperator
pauliXAt bits i = bitOpAt bits i pauliX

phaseOf :: Double -> QOperator
phaseOf theta = Vec.fromList [Vec.fromList [1,0], Vec.fromList [0,mkPolar 1 theta]]

phaseS, phaseT :: QOperator
phaseS = phaseOf (pi/2)
phaseT = phaseOf (pi/4)

-- | Total number of qubits -> position of qubit in question
phaseAt :: Qubits -> Qubits -> Double -> QOperator
phaseAt bits i theta = bitOpAt bits i (phaseOf theta)

phaseSAt, phaseTAt :: Qubits -> Qubits -> QOperator
phaseSAt bits i = bitOpAt bits i phaseS
phaseTAt bits i = bitOpAt bits i phaseT

-----------------------------------------------
-- Controlled operators

-- | Single-bit projection operators
projTo0, projTo1 :: QOperator
projTo0 = Vec.fromList [Vec.fromList [1,0], Vec.fromList [0,0]]
projTo1 = Vec.fromList [Vec.fromList [0,0], Vec.fromList [0,1]]

-- | Controlled operator
-- Use bit (i) to control the action of (op) on bit (j)
-- Bit (j) is unchanged if bit (i) has value 0, and (op) is applied
-- on bit (j) if bit (i) has value 1
cOpAt :: Qubits -> Qubits -> Qubits -> QOperator -> QOperator
cOpAt bits i j op
  | i<j = (bitOpAt i i projTo0 `tensorProdOp` identityOp (j-i) `tensorProdOp` identityOp (bits-j))
          `addOp`
          (bitOpAt i i projTo1 `tensorProdOp` bitOpAt (j-i) (j-i) op `tensorProdOp` identityOp (bits-j))
  | i>j = (identityOp j `tensorProdOp` bitOpAt (i-j) (i-j) projTo0 `tensorProdOp` identityOp (bits-i))
          `addOp`
          (bitOpAt j j op `tensorProdOp` bitOpAt (i-j) (i-j) projTo1 `tensorProdOp` identityOp (bits-i))
  | i==j = error "Cannot control an operation on a bit by the same bit"

cNOTAt :: Qubits -> Qubits -> Qubits -> QOperator
cNOTAt bits i j = cOpAt bits i j pauliX


-------------------------------------------------------
-- Quantum state monads

-- | The state monad representing the quantum computer QState 
-- is the quantum state of the computer, StdGen is a random
-- number generator that is used at quantum measurements
type QComputer = State (QState, StdGen)

-- | The measurement defined here is restricted to projective
-- measurements only (POVM measurement not included)
type QMeasure = QComputer

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

measure :: SpectralDecom -> QMeasure Double
measure op = do
  (psi,_) <- get
  let
    probs = Vec.map (\(eigval, eigvecs) -> (eigval, (^2) . magnitude . List.sum . List.map (`innerProdV` psi) $ eigvecs)) op
  result <- randFromDist probs
  (_,g) <- get
  put (normalizeV $ projectTo (snd . Vec.head $ Vec.filter (\(ev,_) -> ev==result) op) psi, g)
  return result

initialize :: QState -> QComputer ()
initialize vec = do
  (_,g) <- get
  put (vec,g)

applyGate :: QOperator -> QComputer ()
applyGate op = do
  (psi,g) <- get
  put (op `actOn` psi, g)

{-
pauliX_decom :: SpectralDecom
pauliX_decom = Vec.fromList [(1.0,[Vec.fromList [1:+0,1:+0]]),(-1.0,[Vec.fromList [1:+0,(-1):+0]])]
-}
