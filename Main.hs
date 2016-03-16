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
import Control.Monad.Trans
import Control.Monad.State
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

--------------------------------------
-- Option Parsers

data Flags = Flags { resultOnlyF :: Bool , cheatF :: Bool }

fileName :: Parser String
fileName = strArgument (metavar "filename" <> help "filename")

measOnly :: Parser Bool
measOnly = switch (short 'r' <> long "resultonly" <> help "print results for measurements only")

cheat :: Parser Bool
cheat = switch (short 'c' <> long "cheat" <> help "print states after each computation")

inputs :: Parser (String, Flags)
inputs = liftA2 (,) fileName (Flags <$> measOnly <*> cheat)

includeOutput :: Flags -> QComputer QCommand -> QComputer QCommand
includeOutput flags cmd = do
  out <- cmd
  liftIO $ case out of
    Initialize name -> if resultOnlyF flags
                       then return ()
                       else putStrLn $ "Initializing state vector to " ++ name ++ "..."
    Unitary name -> if resultOnlyF flags
                    then return ()
                    else putStrLn $ "Unitary transformation by " ++ name ++ "..."
    MeasureDouble name l -> putStrLn $ "Measured " ++ name
                            ++ ". Result=" ++ show l
    MeasureInt name n -> putStrLn $ "Measured " ++ name
                         ++ ". Result=" ++ show n
    ClassicalData n -> putStrLn $ "Classical data is " ++ show n ++ "."
  if and [cheatF flags, not $ resultOnlyF flags]
    then get >>= liftIO . putStrLn . formatVec 4 . fst
    else return ()
  return out

executeQC :: (String, Flags) -> IO ()
executeQC (file, flag) = do
  f <- openFile file ReadMode
  contents <- hGetContents f
  let processedInput = filter (/=' ') . filter (/='\n') $ contents
  let parsed = readP_to_S readComputation processedInput
  if length parsed == 1
    then do
    g <- newStdGen
    let qcWithIO = sequence $ map (includeOutput flag) (fst . head $ parsed)
    result <- evalQC g qcWithIO
    case result of
     Right _ -> return ()
     Left err -> error err
    else do
    error "No Parse"
  hClose f

-- | Preliminary executable
main = execParser opts >>= executeQC
  where opts = info inputs
               ( fullDesc <>
                 progDesc "Quantum computer simulator:\n Include appropriate input file." <>
                 header "Quantum")
  
