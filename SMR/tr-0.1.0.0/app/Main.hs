-- | Run Haskell tr implementation.
--
-- We will be testing using your `Tr` module, not `Main`, so don't worry too
-- much about compatibility of argument parsing.
module Main where
import Tr
import System.Environment (getArgs)

loop :: String -> String -> String -> IO ()
loop arg1 arg2 line =
  do
    if arg1 == "-d"
    then putStrLn $ tr (arg2) (Just "") (line)
    else putStrLn $ tr (arg1) (Just (arg2)) (line)

    newLine <- getLine

    loop arg1 arg2 newLine


-- | Main - parse args, and read from stdin.
main :: IO ()
main = do
  args <- getArgs

  if length args /= 2
    then putStrLn $ "Incorrect args"
    else
      do
        newLine <- getLine
        loop (args!!0) (args!!1) newLine


