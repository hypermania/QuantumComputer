import Computer
import Parser
import System.Environment
import System.IO
import Text.ParserCombinators.ReadP



-- | Testing
main = do
  args <- getArgs
  f <- openFile (args!!0) ReadMode
  result <- hGetContents f
  putStrLn $ show $ readP_to_S opFromList result
  hClose f
  
