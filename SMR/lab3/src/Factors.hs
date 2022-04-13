module Factors
    (getFactorCount,
      findAllFactors
    ) where


getFactorCount :: Int -> Int -> Int
getFactorCount num divisor
  | mod num divisor == 0 = 1 + getFactorCount (div num divisor) divisor
  | otherwise = 0


getFactor :: Int -> Int -> [Int]
getFactor num divisor
  | num <= 1 = []
  | factorCount > 0 = replicate factorCount divisor ++ getFactor (div num (divisor ^ factorCount)) (divisor + 1)
  | num < divisor * divisor = [num]
  | otherwise = getFactor num (divisor + 1)
  where factorCount = getFactorCount num divisor


findAllFactors :: Int -> [Int]
findAllFactors input
  | input > 0 = getFactor input 2
  | otherwise = getFactor (abs input) 2