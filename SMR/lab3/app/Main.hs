module Main where

import System.Environment (getArgs)

import Factors

newtype NoQuotesStr = NoQuotesStr String
instance Show NoQuotesStr where show (NoQuotesStr str) = str

main :: IO ()
main =
  do
    args <- fmap (map read) getArgs
    mapM_ (\x -> print $ NoQuotesStr (show x ++ " : " ++ show (findAllFactors x))) args
