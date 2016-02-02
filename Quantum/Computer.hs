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

------------------------------------------------------------

-- | Type for the quantum state
-- Indexing of the state vectors usually go from 0 to 2^n-1,
-- where n is the number of qubits
type QState = Vector (Complex Double)

-- | Type for an orthogonal LI set of state vectors
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


type Qubits = Int
                                            
zeroV :: Qubits -> QState
zeroV n = Vec.replicate (2^n) (0:+0)

-- | An eigenstate representing one integer
-- bits -> integer -> state
intV :: Qubits -> Int -> QState
intV bits n = (Vec.replicate n (0:+0)) Vec.++ Vec.singleton (1:+0) Vec.++ (Vec.replicate (2^bits-(n+1)) (0:+0))
