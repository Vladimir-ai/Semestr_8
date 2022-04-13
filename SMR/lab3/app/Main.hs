module Main where

import System.Environment (getArgs)

import Factors


main :: IO ()
main =
  do
    args <- fmap (map read) getArgs
    mapM_ (\x -> putStrLn  $  show x ++ " : " ++ show (findAllFactors x)) args
