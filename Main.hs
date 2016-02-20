import Computer
import Parser
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP
import Data.Char

{-
compute :: QMeasure Double
compute = do
  measure pauliX_decom
  measure pauliX_decom
-}

-- | Testing
main = do
  args <- getArgs
  f <- openFile (args!!0) ReadMode
  contents <- hGetContents f
  let reduced1 = filter (/=' ') . filter (/='\n') $ contents
  let reduced2 = readP_to_S readDefs reduced1
  putStrLn $ show $ last reduced2
  --putStrLn $ show $ readP_to_S opFromList result
  hClose f
  
