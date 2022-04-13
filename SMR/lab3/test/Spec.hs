import Test.Hspec
import Test.QuickCheck

import Factors
import Data.List (findIndex)
import Data.Foldable (find)

main :: IO ()
main = hspec $ describe "Testing factors" $ do
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


      describe "Checking findAllFactors function" $ do
        context "Complex numbers" $ do
          it "100 -> [2, 2, 5, 5]" $
            findAllFactors 100 `shouldBe` [2, 2, 5, 5]

          it "4 -> [2, 2]" $
            findAllFactors 4 `shouldBe` [2, 2]

          it "33 -> [3, 11]" $
            findAllFactors 33 `shouldBe` [3, 11]

        context "Prime numbers" $ do
          it "2 -> [2]" $
            findAllFactors 2 `shouldBe` [2]

          it "23 -> [23]" $
            findAllFactors 23 `shouldBe` [23]

          it "127 -> [127]" $
            findAllFactors 127 `shouldBe` [127]

        context "Empty list result" $ do
          it "1 -> []" $
            findAllFactors 1 `shouldBe` []

          it "0 -> []" $
            findAllFactors 0 `shouldBe` []

        context "Negative numbers" $ do
          it "-1 -> []" $
            findAllFactors (-1) `shouldBe` []

          it "-2 -> [2]" $
            findAllFactors (-2) `shouldBe` [2]

          it "-4 -> [2, 2]" $
            findAllFactors (-4) `shouldBe` [2,2]
