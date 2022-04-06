module Factors
    (isqrt,
      checkNumIsPrime,
      getFactorCount,
      findAllFactors
    ) where
import GHC.Float (int2Double)


isqrt :: Int -> Int
isqrt value = ceiling $ sqrt $ int2Double value


checkNumIsPrime :: Int -> Bool
checkNumIsPrime num
  | num == 2 = True
  | otherwise = null [x | x <- [2..isqrt num], mod num x == 0]


getFactorCount :: Int -> Int -> Int
getFactorCount num divisor =
  case mod num divisor of
    0 -> 1 + getFactorCount (div num divisor) divisor
    _ -> 0


getFactor :: Int -> Int -> [Int]
getFactor num divisor
  | num == 1 = []
  | checkNumIsPrime num = [num]
  | otherwise = if checkNumIsPrime divisor && factorCount > 0
                  then replicate factorCount divisor ++ getFactor (div num (divisor ^ factorCount)) (divisor + 1)
                  else getFactor num (divisor + 1)
                  where factorCount = getFactorCount num divisor


findAllFactors :: Int -> [Int]
findAllFactors input =
  getFactor input 2
