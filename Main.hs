{-|
Module      : Main
Description : Executable for a quantum computer
Copyright   : Siyang Ling, 2016
License     : none
Maintainer  : hypermania@uchicago.edu
Stability   : experimental
Portability : Unknown

The main executable.
-}
import Computer
import Parser
import System.Environment
import System.IO
import System.Random
import Control.Monad
import Data.Complex
import qualified Data.Vector as Vec
import Data.Maybe
import Data.List as List
import Options.Applicative

---------------------------------------
-- Formatting

-- |
-- > formatVec n vec
-- Formatting vector output. n is for number of significant figures.
formatVec :: Int -> QState -> String
formatVec n vec = (concat $ map (truncStr (2*n+4) . show) $ enumFromTo 0 (length coeffList-1))
                  ++ '\n':(concat $ map (truncCoeff n) coeffList) ++ "\n"
  where truncStr n str
          | length str<n = truncStr n (str++" ")
          | length str>n = truncStr n (init str)
          | length str==n = str
        truncCoeff n c = (truncStr n . show . realPart $ c) ++ "+"
                         ++ (truncStr n . show . imagPart $ c) ++ "i  "
        coeffList = Vec.toList vec

formatResult :: QCommand -> String
formatResult result = case result of
  Initialize -> "Initialize state vector.\n"
  Unitary -> "Perform unitary transformation.\n"
  MeasureDouble x -> "Measured: " ++ show x ++ "\n"
  MeasureInt n -> "Measured: " ++ show n ++ "\n"

formatResultQuiet :: QCommand -> String
formatResultQuiet result = case result of
  Initialize -> []
  Unitary -> []
  MeasureDouble x -> "Measured: " ++ show x ++ "\n"
  MeasureInt n -> "Measured: " ++ show n ++ "\n"

--------------------------------------
-- Option Parsers

fileName :: Parser String
fileName = strArgument (metavar "filename" <> help "filename")

resultOnly :: Parser Bool
resultOnly = flag False True (short 'r' <> long "resultonly" <> help "print results for measurements only")

cheat :: Parser Bool
cheat = flag False True (short 'c' <> long "cheat" <> help "print states after each computation")

inputs :: Parser (String, Bool, Bool)
inputs = liftA3 (,,) fileName resultOnly cheat

printCMD :: [QCommand] -> [String]
printCMD [] = []
printCMD (cmd:xs) = case cmd of
  Initialize -> "Initializing to vector...":printCMD xs
  Unitary -> "Apply unitary operator...":printCMD xs
  MeasureDouble l -> ("Measured: " ++ show l):printCMD xs
  MeasureInt n -> ("Measured: " ++ show n):printCMD xs

printResult :: [QCommand] -> [String]
printResult (cmd:xs) = case cmd of
  MeasureDouble l -> ("Measured: " ++ show l):printCMD xs
  MeasureInt n -> ("Measured: " ++ show n):printCMD xs

process :: Bool -> Either String [QCommand] -> String
process _ (Left str) = "Input file invalid\n" ++ str
process r (Right list) = join $ intersperse "\n" $
                         (if r then printResult else printCMD) list

execute :: (String, Bool, Bool) -> IO ()
execute (file, r, c) = do
  f <- openFile file ReadMode
  contents <- hGetContents f
  let processed = filter (/=' ') . filter (/='\n') $ contents
  g <- newStdGen
  putStrLn $ process r $ runInput g processed
  hClose f

-- | Preliminary executable
main = execParser opts >>= execute
  where opts = info inputs
               ( fullDesc <>
                 progDesc "Quantum computer simulator" <>
                 header "Quantum")
  
