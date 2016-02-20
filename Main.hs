import Computer
import Parser
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Complex
import qualified Data.Vector as Vec

{-
compute :: QMeasure Double
compute = do
  measure pauliX_decom
  measure pauliX_decom
-}


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
  
