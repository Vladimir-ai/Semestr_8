import Test.Hspec
import Test.QuickCheck

import Factors
import Data.List (findIndex)
import Data.Foldable (find)

main :: IO ()
main = hspec $ describe "Testing factors" $ do
      describe "Checking isqrt function" $ do
        context "Integer result" $ do
          it "4 -> 2" $
            isqrt 4 `shouldBe` 2

          it "100 -> 10" $
            isqrt 100 `shouldBe` 10

        context "Fractional result" $ do
          it "10 -> 4" $
            isqrt 10 `shouldBe` 4

          it "101 -> 11" $
            isqrt 101 `shouldBe` 11


      describe "Checking getFactorCount function" $ do
        context "Multiple factors" $ do
          it "4 2 -> 2" $
            getFactorCount 4 2 `shouldBe` 2

          it "1000 10 -> 3" $
            getFactorCount  1000 10 `shouldBe` 3

        context "Zero factors" $ do
          it "5 2 -> 0" $
            getFactorCount 5 2 `shouldBe` 0

          it "101 10 -> 0" $
            getFactorCount 101 10 `shouldBe` 0


      describe "checking checkNumIsPrime function" $ do
        context "Primes" $ do
          it "2 -> True" $
            checkNumIsPrime 2 `shouldBe` True

          it "3 -> True" $
            checkNumIsPrime 3 `shouldBe` True

          it "101 -> True" $
            checkNumIsPrime 101 `shouldBe` True

        context "Not primes" $ do
          it "10 -> False" $
            checkNumIsPrime 10 `shouldBe` False

          it "2000 -> False" $
            checkNumIsPrime 2000 `shouldBe` False

          it "4 -> False" $
            checkNumIsPrime 4 `shouldBe` False


      describe "Checking findAllFactors function" $ do
        context "Complex numbers" $ do
          it "100 -> [2, 2, 5, 5]" $
            findAllFactors 100 `shouldBe` [2, 2, 5, 5]

          it "4 -> [2, 2]" $
            findAllFactors 4 `shouldBe` [2, 2]

          it "33 -> [3, 11]" $
            findAllFactors 33 `shouldBe` [3, 11]

        context "Prime numbers" $ do
          it "23 -> 23" $
            findAllFactors 23 `shouldBe` [23]

          it "127 -> 127" $
            findAllFactors 127 `shouldBe` [127]