module Factors
    (getFactorCount,
      findAllFactors
    ) where


getFactorCount :: Int -> Int -> Int
getFactorCount num divisor =
  helper num divisor 0
  where
    helper :: Int -> Int -> Int -> Int
    helper 0 _ count = count
    helper number divisr count
      | mod number divisr == 0 = helper (div number divisr) divisr (count + 1)
      | otherwise = helper 0 divisr count


getFactor :: Int -> Int -> [Int]
getFactor num divisor =
  helper num divisor []
  where
    helper :: Int -> Int -> [Int] -> [Int]
    helper 1 _ acc = acc
    helper 0 _ acc = acc
    helper number divisr acc
      | factorCount > 0 = helper (div number (divisr ^ factorCount)) (divisr + 1) (acc ++ replicate factorCount divisr)
      | number < divisr * divisr = helper 1 (divisr + 1) (acc ++ [number])
      | otherwise = helper number (divisr + 1) acc
      where factorCount = getFactorCount number divisr



findAllFactors :: Int -> [Int]
findAllFactors input
  | input > 0 = getFactor input 2
  | otherwise = getFactor (abs input) 2