module Factors
    (checkNumIsPrime,
      getFactorCount,
      findAllFactors
    ) where


checkNumIsPrime :: Int -> Bool
checkNumIsPrime num
  | num == 2 = True
  | otherwise = not $ any divisible $ takeWhile notTooBig [2..]
  where
    divisible y = mod num y == 0
    notTooBig y = y * y <= num


getFactorCount :: Int -> Int -> Int
getFactorCount num divisor
  | mod num divisor == 0 = 1 + getFactorCount (div num divisor) divisor
  | otherwise = 0


getFactor :: Int -> Int -> [Int]
getFactor num divisor
  | num == 1 = []
  | checkNumIsPrime divisor && factorCount > 0 = replicate factorCount divisor ++ getFactor (div num (divisor ^ factorCount)) (divisor + 1)
  | checkNumIsPrime num = [num]
  | otherwise = getFactor num (divisor + 1)
  where factorCount = getFactorCount num divisor


findAllFactors :: Int -> [Int]
findAllFactors input
  | input > 0 = getFactor input 2
  | otherwise = getFactor (abs input) 2