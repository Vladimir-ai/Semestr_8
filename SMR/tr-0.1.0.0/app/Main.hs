-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where
import Tr
import System.Environment (getArgs)

-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- getArgs
  if length args == 3
  then
    if args!!0 == "-d"
    then putStrLn $ tr (args!!1) (Just "") (args!!2)
    else putStrLn $ tr (args!!0) (Just (args!!1)) (args!!2)
  else putStrLn $ "Incorrect args"
