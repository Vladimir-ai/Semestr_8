module Factors
    (getFactorCount,
      findAllFactors
    ) where


getFactorCount :: Int -> Int -> Int
getFactorCount num divisor =
  helper num divisor 0
  where
    helper :: Int -> Int -> Int -> Int
    helper number divisr count
      | mod number divisr == 0 = helper (div number divisr) divisr (count + 1)
      | otherwise = count


getFactor :: Int -> Int -> [Int]
getFactor num divisor =
  helper num divisor []
  where
    helper :: Int -> Int -> [Int] -> [Int]
    helper number divisr acc
      | number <= 1 = acc
      | factorCount > 0 = helper (div number (divisr ^ factorCount)) (divisr + 1) (acc ++ replicate factorCount divisr)
      | number < divisr * divisr = acc ++ [number]
      | otherwise = helper number (divisr + 1) acc
      where factorCount = getFactorCount number divisr



findAllFactors :: Int -> [Int]
findAllFactors input
  | input > 0 = getFactor input 2
  | otherwise = getFactor (abs input) 2