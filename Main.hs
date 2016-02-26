import Computer
import Parser
import System.Environment
import System.IO
import System.Random
import Control.Monad
import Data.Complex
import qualified Data.Vector as Vec

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


-- | Preliminary executable
main = do
  args <- getArgs
  replicateM (read (args!!1)) $
    if length args == 0
    then error "no input argument"
    else do
      f <- openFile (args!!0) ReadMode
      contents <- hGetContents f
      let processed = filter (/=' ') . filter (/='\n') $ contents
      g <- newStdGen
      putStrLn $ show $ runInput g processed
      hClose f

