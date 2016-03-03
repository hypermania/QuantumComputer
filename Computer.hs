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
module Computer (
  -- * Types
  QState,
  ONB,
  Qubits,
  QOperator,
  SpectralDecom,

  -- * Functions
  -- ** on vectors
  addV,
  scalV,
  normV,
  normalizeV,
  innerProdV,
  tensorProdV,
  projectTo,
  
  actOn,
  
  -- ** on operators
  addOp,
  scalOp,
  multOp,
  transposeOp,
  conjugateOp,
  hConjugateOp,
  tensorProdOp,
  commutatorOp,

  -- * Construction
  -- ** vectors
  zeroV,
  intV,
  initV,
  superposedIntV,
  
  -- ** Operators
  baseOp,  identityOp,
  -- *** for one-qubit systems
  pauliI, pauliX, pauliY, pauliZ,
  phaseOf,
  phaseS, phaseT,
  -- *** single-bit operators
  bitOpAt,
  hadamardOpAt,
  pauliXAt, phaseAt, phaseSAt, phaseTAt,
  -- *** multi-bit operators
  hadamardOpFull,
  -- *** controlled operators
  controlledOpAt,
  cnotAt,
  -- *** high level operators
  qftNaive,
  qftBitNaive,
  
  -- * Quantum State Monads
  QComputer,
  QCommand (Initialize, Unitary, MeasureDouble, MeasureInt),
  initialize,
  applyGate,
  spectMeasure, numberMeasure,  
  evalQC,
  runQC

  -- ** Using QComputer Monad
  -- $intro
  )
       where

import qualified Data.List as List
import Data.Vector as Vec
import Data.Complex
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative

-- ----------------------------------------------------------
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

-- | Add two operators
addOp :: QOperator -> QOperator -> QOperator
addOp = Vec.zipWith addV

-- | Scale an operator
scalOp :: Complex Double -> QOperator -> QOperator
scalOp k = (<$>) (k `scalV`) 

-- | Multiply two operators
multOp :: QOperator -> QOperator -> QOperator
multOp op = Vec.map (op `actOn`)

-- | Transpose of an operator
transposeOp :: QOperator -> QOperator
transposeOp op = Vec.map (\n -> Vec.map (!n) op) $ Vec.enumFromTo 0 (rows-1)
  where rows = Vec.length $ op ! 0

-- | Complex conjugate of an operator
conjugateOp :: QOperator -> QOperator
conjugateOp = (fmap . fmap) conjugate

-- | The hermitian conjugate
hConjugateOp :: QOperator -> QOperator
hConjugateOp = transposeOp . conjugateOp

-- | Tensor product for an operator
tensorProdOp :: QOperator -> QOperator -> QOperator
tensorProdOp a b = Vec.map (\k -> tensorProdV
                                  (a ! (k `div` q))
                                  (b ! (k `mod` q)))
                   $ Vec.enumFromTo 0 (p*q-1)
  where p = Vec.length a
        q = Vec.length b

-- | Commutator of two operators
commutatorOp :: QOperator -> QOperator -> QOperator
commutatorOp op1 op2 = (op1 `multOp` op2) `addOp` scalOp (-1) (op2 `multOp` op1)

{-
-- | Check if an operator is Hermitian
isHermitian :: QOperator -> Bool
isHermitian op = hConjugateOp op == op


-- | Check if an operator if Unitary
isUnitary :: QOperator -> Bool
isUnitary op = isSquare && isNormalized && isOrthogonal
  where
    isSquare = rows == columns
    isNormalized = Vec.and . Vec.map ((==1.0) . normV) $ op
    isOrthogonal = Vec.and . Vec.map (\c -> Vec.and . Vec.map ((==0.0) . (op!c `innerProdV`)) $ Vec.drop (c+1) op) $ Vec.enumFromTo 0 (columns-1)
    rows = Vec.length $ op!0
    columns = Vec.length op
-}

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

-- | The identity operator
identityOp :: Qubits -> QOperator
identityOp bits = iterate (`tensorProdOp` pauliI) baseOp !! bits

hadamardBase :: QOperator
hadamardBase = (1/sqrt 2.0) `scalOp` Vec.fromList [Vec.fromList [1,1], Vec.fromList [1,-1]]

-- | The hadamard operator on all bits
hadamardOpFull :: Qubits -> QOperator
hadamardOpFull bits = iterate (`tensorProdOp` hadamardBase) baseOp !! bits

-- | Input the total number of qubits (n) and an index (i) for a qubit
-- give an operator that performs hadamard gate on qubit i.
hadamardOpAt :: Qubits -> Qubits -> QOperator
hadamardOpAt bits i = bitOpAt bits i hadamardBase

-- | Input the total number of qubits (n) and an index (i) for a qubit
-- give an operator that performs NOT gate on qubit i.
pauliXAt :: Qubits -> Qubits -> QOperator
pauliXAt bits i = bitOpAt bits i pauliX

-- | Input: the relative phase t.
-- The operator maps |1> -> e^(it)|1> 
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
controlledOpAt :: Qubits -> Qubits -> Qubits -> QOperator -> QOperator
controlledOpAt bits i j op
  | i<j = (bitOpAt i i projTo0 `tensorProdOp` identityOp (j-i) `tensorProdOp` identityOp (bits-j))
          `addOp`
          (bitOpAt i i projTo1 `tensorProdOp` bitOpAt (j-i) (j-i) op `tensorProdOp` identityOp (bits-j))
  | i>j = (identityOp j `tensorProdOp` bitOpAt (i-j) (i-j) projTo0 `tensorProdOp` identityOp (bits-i))
          `addOp`
          (bitOpAt j j op `tensorProdOp` bitOpAt (i-j) (i-j) projTo1 `tensorProdOp` identityOp (bits-i))
  | i==j = error "Cannot control an operation on a bit by the same bit"


-- | 
-- > cnotAt bits i j
-- For a (bits)-qubit system, using i-th qubit as the controlling qubit,
-- perform NOT gate at j-th qubit.
cnotAt :: Qubits -> Qubits -> Qubits -> QOperator
cnotAt bits i j = controlledOpAt bits i j pauliX

------------------------------------------------------
-- High level operations

-- | Quantum Fourier Transform (Naive implementation)
-- Input: dimension of vector space
qftNaive :: Int -> QOperator
qftNaive n = (1/sqrt (fromIntegral n) :+ 0) `scalOp`
             (Vec.map col $ Vec.enumFromTo 0 (n-1))
  where root = mkPolar 1 (2*pi/fromIntegral n)
        col i = Vec.map ((^i) . (root ^)) $ Vec.enumFromTo 0 (n-1)

-- | Fast fourier transform (copied from Nielson)
-- TODO
fastqft :: Qubits -> QOperator
fastqft bits = Vec.foldr1 multOp (Vec.map opAtBit $ Vec.enumFromTo 1 bits)
  where opAtBit n = identityOp (n-1) `tensorProdOp`
                    hadamardOpAt 1 1 `tensorProdOp`
                    undefined
        r remain i j = controlledOpAt remain j i (phaseOf (1/2^(j-i+1)) )

-- | QFT with boundaries
qftBitNaive :: Qubits -> Qubits -> Qubits -> QOperator
qftBitNaive bits i j = identityOp (i-1) `tensorProdOp`
                       qftNaive (2^(j-i+1)) `tensorProdOp`
                       identityOp (bits-j)


-- -----------------------------------------------------
--  Quantum state monads

-- | The monad representing the quantum computer.
-- State monad is used for representing the quantum state.
type QComputer = ExceptT String (RandT StdGen (StateT QState IO))

-- | A probability distribution of a
type Distribution a = Vector (a, Double)

-- | Various commands a quantum computer takes.
-- For measurements, the value of measurement is also returned (in wrapped form).
data QCommand = Initialize
              | Unitary
              | MeasureDouble Double
              | MeasureInt Int
              deriving Show

-- | Evaluate/run a QComputer monad.
-- In case of an error, an error message is returned.
evalQC :: StdGen -> QComputer a -> IO (Either String a)
evalQC g qc = evalStateT (evalRandT (runExceptT qc) g) (zeroV 0)

runQC :: StdGen -> QComputer a -> IO (Either String a, QState)
runQC g qc = runStateT (evalRandT (runExceptT qc) g) (zeroV 0)

-- | Initialize the quantum computer to the given state
initialize :: QState -> QComputer QCommand
initialize vec = put vec >> return Initialize

checkOpSize :: QOperator -> QComputer ()
checkOpSize op = do
  psi <- get
  if Vec.length op == Vec.length psi
    then return ()
    else throwError "Operator size do not match state vector"

-- | Apply a given operator 
applyGate :: QOperator -> QComputer QCommand
applyGate op = do
  checkOpSize op
  psi <- get
  put $ op `actOn` psi
  return Unitary

-- | Given probability distribution of a, choose a random instance of a
-- similar to implementation for fromList in Control.Monad.Random
fromDist :: Distribution a -> QComputer a
fromDist xs
  | Vec.length xs == 0 = throwError "randFromDist' called from empty vector"
  | Vec.length xs == 1 = return . fst . Vec.head $ xs
  | otherwise = do
      let
        s = Vec.sum (Vec.map snd xs)
        cs = Vec.scanl1 (\(_,summed) (num,nextProb) -> (num,summed+nextProb)) xs
      if s==0 then throwError "probability sum to 0" else return ()
      p <- getRandomR (0.0, s)
      return . fst . Vec.head $ Vec.dropWhile (\(_,q) -> q<p) cs

-- | Given spectral decomposition, give the probability distribution of (eigenvalues, eigenvectors).
spectDist :: SpectralDecom -> QComputer (Distribution (Double, ONB))
spectDist spect = do
  psi <- get
  return $ Vec.map
    (\(val, onb) -> 
      ((val, onb), (^2) . magnitude . List.sum . List.map (`innerProdV` psi) $ onb)) spect
    
-- | Quantum measurement with respect to a SpectralDecom.
spectMeasure :: SpectralDecom -> QComputer QCommand
spectMeasure spect = do
  psi <- get
  (val, onb) <- spectDist spect >>= fromDist
  put $ normalizeV $ projectTo onb psi
  return $ MeasureDouble val

-- | Quantum measurement of the number value of the quantum state.
numberMeasure :: QComputer QCommand
numberMeasure = do
  psi <- get
  let probs = Vec.zipWith (,) (Vec.enumFromN 0 (Vec.length psi)) (Vec.map ((^2) . magnitude) psi)
  result <- fromDist probs
  put $ intV (round $ log (fromIntegral $ Vec.length psi) / log 2) result
  return $ MeasureInt result

{- $intro

> compute = do
>   initialize $ intV 1 0
>   apply $ hadamardOpFull 1
>   numberMeasure

... is a monad that does what it says. 

-}
